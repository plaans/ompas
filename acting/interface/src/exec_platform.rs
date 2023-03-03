use crate::lisp_domain::LispDomain;
use ompas_language::interface::{
    LOG_TOPIC_PLATFORM, PROCESS_GET_UPDATES, PROCESS_SEND_COMMANDS, PROCESS_START_PLATFORM,
    PROCESS_TOPIC_PLATFORM,
};

use crate::platform_config::PlatformConfig;
use crate::platform_interface::command_request::Request;
use crate::platform_interface::command_response::Response;
use crate::platform_interface::platform_interface_client::PlatformInterfaceClient;
use crate::platform_interface::{
    event, CommandCancelRequest, CommandExecutionRequest, CommandRequest, CommandResponse,
    Expression, InitGetUpdate, PlatformUpdate, ResourceKind, StateVariableType,
};
use crate::{platform_interface, PlatformDescriptor, TOKIO_CHANNEL_SIZE};
use async_trait::async_trait;
use ompas_language::process::PROCESS_TOPIC_OMPAS;
use ompas_middleware::logger::LogClient;
use ompas_middleware::{Master, ProcessInterface};
use ompas_structs::acting_manager::action_status::ProcessStatus;
use ompas_structs::acting_manager::ActingManager;
use ompas_structs::execution::resource::Capacity;
use ompas_structs::state::partial_state::PartialState;
use ompas_structs::state::world_state::StateType;
use platform_interface::platform_update::Update;
use sompas_structs::lmodule::LModule;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::borrow::Borrow;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};

#[derive(Clone)]
pub struct ExecPlatform {
    inner: Arc<RwLock<dyn PlatformDescriptor>>,
    acting_manager: ActingManager,
    command_stream: Arc<Mutex<Option<tokio::sync::mpsc::Sender<CommandRequest>>>>,
    log: LogClient,
    pub config: Arc<RwLock<PlatformConfig>>,
}

impl ExecPlatform {
    pub async fn new(
        inner: Arc<RwLock<dyn PlatformDescriptor>>,
        acting_manager: ActingManager,
        command_stream: Arc<Mutex<Option<tokio::sync::mpsc::Sender<CommandRequest>>>>,
        log: LogClient,
        config: Arc<RwLock<PlatformConfig>>,
    ) -> Self {
        Master::set_child_process(PROCESS_TOPIC_PLATFORM, PROCESS_TOPIC_OMPAS).await;
        Master::set_child_process(PROCESS_TOPIC_OMPAS, PROCESS_TOPIC_PLATFORM).await;

        Self {
            inner,
            acting_manager,
            command_stream,
            log,
            config,
        }
    }

    pub async fn exec_command(&self, command: &[LValue], command_id: usize) {
        let mut arguments: Vec<Expression> = vec![];
        for arg in command {
            arguments.push(LValueS::try_from(arg).unwrap().try_into().unwrap())
        }

        self.log
            .debug(format!("New command request: {}", LValue::from(command)))
            .await;

        let request = CommandRequest {
            request: Some(Request::Execution(CommandExecutionRequest {
                arguments,
                command_id: command_id as u64,
            })),
        };
        self.command_stream
            .lock()
            .await
            .as_ref()
            .unwrap()
            .send(request)
            .await
            .unwrap_or_else(|_| todo!());
    }

    pub async fn cancel_command(&self, command_id: usize) {
        let request = CommandRequest {
            request: Some(Request::Cancel(CommandCancelRequest {
                command_id: command_id as u64,
            })),
        };
        self.command_stream
            .lock()
            .await
            .as_ref()
            .unwrap()
            .send(request)
            .await
            .unwrap_or_else(|_| todo!());
    }

    async fn get_updates(
        mut client: PlatformInterfaceClient<tonic::transport::Channel>,
        acting_manager: ActingManager,
    ) {
        let mut process_interface: ProcessInterface = ProcessInterface::new(
            PROCESS_GET_UPDATES,
            PROCESS_TOPIC_PLATFORM,
            LOG_TOPIC_PLATFORM,
        )
        .await;
        let request = tonic::IntoRequest::into_request(InitGetUpdate {});

        process_interface.log_info("Initiating update stream").await;

        let stream = match client.get_updates(request).await {
            Ok(s) => s,
            Err(e) => {
                process_interface
                    .log_error(format!("Error starting update stream: {e}"))
                    .await;
                process_interface.kill(PROCESS_TOPIC_PLATFORM).await;
                return;
            }
        };
        let mut stream: tonic::codec::Streaming<PlatformUpdate> = stream.into_inner();

        loop {
            tokio::select! {
                _ = process_interface.recv() => {
                    break;
                }
                msg = stream.message() => {
                    //println!("[PlatformClient] received new update: {:?}", msg);
                    match msg {
                        Err(err) => {
                                process_interface.log_error(format!("Ggpc error: {err}")).await;
                        }
                        Ok(None) => {
                            process_interface.log_error("Grpc stream closed").await;
                            process_interface.kill(PROCESS_TOPIC_PLATFORM).await;
                        }
                        Ok(Some(msg)) => {
                            if let Some(update) =  msg.update {
                                match update {
                                    Update::State(state) => {
                                        let mut r#static =  PartialState {
                                            inner: Default::default(),
                                            _type: Some(StateType::Static)
                                        };

                                        let mut dynamic = PartialState {
                                            inner: Default::default(),
                                            _type: Some(StateType::Dynamic)
                                        };

                                        for sv in state.state_variables {

                                            let mut key : Vec<LValueS> = vec![sv.state_function.into()];
                                            for p in sv.parameters {
                                                key.push(p.borrow().try_into().unwrap());
                                            }
                                            match StateVariableType::from_i32(sv.r#type).unwrap() {
                                                StateVariableType::Static => {
                                                    r#static.insert(key.into(), sv.value.unwrap().borrow().try_into().unwrap())
                                                }
                                                StateVariableType::Dynamic => {
                                                    dynamic.insert(key.clone().into(), sv.value.unwrap().borrow().try_into().unwrap_or_else(|_| panic!("error on state variable {:#?}", key)))
                                                }
                                            }
                                        }
                                            //println!("[PlatformClient] updating state");
                                            acting_manager.state.update_state(r#static).await;
                                            acting_manager.state.update_state(dynamic).await;
                                    }
                                    Update::Event(event) => {
                                        match event.event {
                                            None => {}
                                            Some(event::Event::Resource(resource)) => {
                                                let label = resource.label;
                                                let capacity: Capacity = match ResourceKind::from_i32(resource.resource_kind) {
                                                    Some(ResourceKind::Unary) => {
                                                        1
                                                    }
                                                    Some(ResourceKind::Divisible) => {
                                                        resource.quantity as usize
                                                    }
                                                    None => {
                                                        panic!()
                                                    }
                                                };
                                                acting_manager.resource_manager.new_resource(label , Some(capacity)).await
                                            }
                                            Some(event::Event::Instance(instance)) => {
                                                acting_manager.state.add_instance(&instance.object, &instance.r#type).await;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    async fn send_commands(
        mut client: PlatformInterfaceClient<tonic::transport::Channel>,
        acting_manager: ActingManager,
        command_stream: tokio::sync::mpsc::Receiver<CommandRequest>,
    ) {
        let mut process = ProcessInterface::new(
            PROCESS_SEND_COMMANDS,
            PROCESS_TOPIC_PLATFORM,
            LOG_TOPIC_PLATFORM,
        )
        .await;
        let stream = tokio_stream::wrappers::ReceiverStream::new(command_stream);

        process.log_info("initiating command stream").await;

        let stream = match client.send_commands(tonic::Request::new(stream)).await {
            Ok(s) => s,
            Err(e) => {
                process
                    .log_error(format!("Error starting command stream: {e}"))
                    .await;
                process.kill(PROCESS_TOPIC_PLATFORM).await;
                return;
            }
        };
        let mut stream = stream.into_inner();

        loop {
            tokio::select! {
                _ = process.recv() => {
                    break;
                }
                msg = stream.message() => {
                    match msg {
                        Err(err) => {
                                process.log_error(format!("Ggpc error: {err}")).await;
                        }
                        Ok(None) => {
                            process.log_error("Grpc stream closed").await;
                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                        }
                        Ok(Some(command_response)) => {
                            let command_response: CommandResponse = command_response;
                            if let Some(response) = command_response.response {
                                let (id, status) = match response {
                                    Response::Accepted(r) => {
                                    (r.command_id as usize, ProcessStatus::Accepted)

                                }
                                Response::Rejected(r) => {
                                    (r.command_id as usize, ProcessStatus::Rejected)

                                }
                                Response::Progress(f) => {
                                    (
                                            f.command_id as usize,
                                            ProcessStatus::Running(Some(f.progress)),
                                        )
                                }
                                Response::Result(r) => match r.result {
                                    true => {
                                        (r.command_id as usize, ProcessStatus::Success)

                                    }
                                    false => {
                                        (r.command_id as usize, ProcessStatus::Failure)

                                    }
                                },
                                Response::Cancelled(c) => {
                                    (
                                            c.command_id as usize,
                                            ProcessStatus::Cancelled(c.result),
                                        )

                                }
                                };
                                acting_manager.set_status(&id, status).await;
                            }
                        }
                    }
                }
            }
        }
    }
}

#[async_trait]
impl PlatformDescriptor for ExecPlatform {
    ///Launch the platform (such as the simulation in godot) and open communication
    async fn start(&self, _: PlatformConfig) {
        let process = ProcessInterface::new(
            PROCESS_START_PLATFORM,
            PROCESS_TOPIC_PLATFORM,
            LOG_TOPIC_PLATFORM,
        )
        .await;

        self.inner
            .write()
            .await
            .start(self.config.read().await.clone())
            .await;

        let server: SocketAddr = self.socket().await;
        let socket = format!("https://{}", server);
        process.log_info(format!("socket: {socket}")).await;
        //println!("server addr: {}", server);
        let client: PlatformInterfaceClient<_> =
            match PlatformInterfaceClient::connect(socket).await {
                Ok(c) => c,
                Err(e) => {
                    process
                        .log_error(format!("Error connecting to client: {:?}", e))
                        .await;
                    process.kill(PROCESS_TOPIC_PLATFORM).await;
                    return;
                }
            };

        let client2 = client.clone();
        let acting_manager = self.acting_manager.clone();
        tokio::spawn(async move {
            ExecPlatform::get_updates(client, acting_manager).await;
        });

        let acting_manager = self.acting_manager.clone();
        let (tx, command_stream) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);
        *self.command_stream.lock().await = Some(tx);
        tokio::spawn(async move {
            ExecPlatform::send_commands(client2, acting_manager, command_stream).await;
        });
    }

    ///Stops the platform.
    async fn stop(&self) {
        self.inner.write().await.stop().await;
    }

    ///Returns the domain of the platform
    async fn domain(&self) -> LispDomain {
        self.inner.read().await.domain().await
    }

    ///Returns a module loaded into the evaluation environment with other bindings
    async fn module(&self) -> Option<LModule> {
        self.inner.read().await.module().await
    }

    ///Returns the server info in order to connect OMPAS to the platform using grpc services
    async fn socket(&self) -> SocketAddr {
        self.inner.read().await.socket().await
    }
}
