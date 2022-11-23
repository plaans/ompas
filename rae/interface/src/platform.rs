use crate::platform_interface::command_request::Request;
use crate::platform_interface::command_response::Response;
use crate::platform_interface::platform_interface_client::PlatformInterfaceClient;
use crate::platform_interface::{
    event, CommandCancelRequest, CommandExecutionRequest, CommandRequest, CommandResponse,
    Expression, InitGetUpdate, PlatformUpdate, StateVariableType,
};
use crate::{platform_interface, TOKIO_CHANNEL_SIZE};
use crate::{LOG_TOPIC_PLATFORM, PROCESS_TOPIC_PLATFORM};
use async_trait::async_trait;
use map_macro::set;
use ompas_middleware::ompas_log::{LogMessage, Logger};
use ompas_middleware::{LogLevel, ProcessInterface};
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::state::action_status::CommandStatus;
use ompas_rae_structs::state::partial_state::PartialState;
use ompas_rae_structs::state::world_state::{StateType, WorldState};
use platform_interface::platform_update::Update;
use sompas_structs::documentation::Documentation;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::any::Any;
use std::borrow::Borrow;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;

const PROCESS_GET_UPDATES: &str = "__PROCESS_GET_UPDATES__";
const PROCESS_SEND_COMMANDS: &str = "__PROCESS_SEND_COMMANDS__";
const PLATFORM_CLIENT: &str = "PlatformClient";

#[derive(Clone)]
pub struct Platform {
    pub inner: Arc<tokio::sync::RwLock<dyn PlatformDescriptor>>,
    pub state: WorldState,
    pub agenda: Agenda,
    pub command_stream: Arc<Mutex<Option<tokio::sync::mpsc::Sender<CommandRequest>>>>,
}

impl Platform {
    pub async fn exec_command(&self, command: &[LValue], command_id: usize) {
        let mut arguments: Vec<Expression> = vec![];
        for arg in command {
            arguments.push(LValueS::try_from(arg).unwrap().try_into().unwrap())
        }
        let topic = Logger::subscribe_to_topic(LOG_TOPIC_PLATFORM).await;

        Logger::log(LogMessage::new(
            LogLevel::Debug,
            PLATFORM_CLIENT,
            topic,
            format!("New command request: {}", LValue::from(command)),
        ))
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
        world_state: WorldState,
    ) {
        let mut process_interface: ProcessInterface =
            ProcessInterface::new(PROCESS_GET_UPDATES, set! {PROCESS_TOPIC_PLATFORM}).await;
        process_interface
            .subscribe_to_log_topic(LOG_TOPIC_PLATFORM)
            .await;
        let request = tonic::IntoRequest::into_request(InitGetUpdate {});

        process_interface
            .log("Initiating update stream", LogLevel::Info)
            .await;

        let stream = client.get_updates(request).await.expect("");
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
                                process_interface.log(format!("Ggpc error: {}",err), LogLevel::Error).await;
                        }
                        Ok(None) => {
                            process_interface.log("Grpc stream closed", LogLevel::Error).await;
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
                                                    dynamic.insert(key.into(), sv.value.unwrap().borrow().try_into().unwrap())
                                                }
                                            }
                                        }
                                            //println!("[PlatformClient] updating state");
                                            world_state.update_state(r#static).await;
                                            world_state.update_state(dynamic).await;
                                    }
                                    Update::Event(event) => {
                                        match event.event {
                                            None => {}
                                            Some(event::Event::Instance(instance)) => {
                                                world_state.add_instance(instance.object, instance.r#type).await;
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
        agenda: Agenda,
        command_stream: tokio::sync::mpsc::Receiver<CommandRequest>,
    ) {
        let mut process =
            ProcessInterface::new(PROCESS_SEND_COMMANDS, set! {PROCESS_TOPIC_PLATFORM}).await;
        let stream = tokio_stream::wrappers::ReceiverStream::new(command_stream);
        process.subscribe_to_log_topic(LOG_TOPIC_PLATFORM).await;

        process
            .log("initiating command stream", LogLevel::Info)
            .await;

        let stream = client
            .send_commands(tonic::Request::new(stream))
            .await
            .expect("");
        let mut stream = stream.into_inner();

        loop {
            tokio::select! {
                _ = process.recv() => {
                    break;
                }
                msg = stream.message() => {
                    match msg {
                        Err(err) => {
                                process.log(format!("Ggpc error: {}",err), LogLevel::Error).await;
                        }
                        Ok(None) => {
                            process.log("Grpc stream closed", LogLevel::Error).await;
                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                        }
                        Ok(Some(command_response)) => {
                            let command_response: CommandResponse = command_response;
                            match command_response.response {
                                None => {}
                                Some(Response::Accepted(r)) => {
                                    agenda
                                        .update_status(&(r.command_id as usize), CommandStatus::Accepted.into())
                                        .await;
                                }
                                Some(Response::Rejected(r)) => {
                                    agenda
                                        .update_status(&(r.command_id as usize), CommandStatus::Rejected.into())
                                        .await;
                                }
                                Some(Response::Progress(f)) => {
                                    agenda
                                        .update_status(
                                            &(f.command_id as usize),
                                            CommandStatus::Progress(f.progress).into(),
                                        )
                                        .await;
                                }
                                Some(Response::Result(r)) => match r.result {
                                    true => {
                                        agenda
                                            .update_status(&(r.command_id as usize), CommandStatus::Success.into())
                                            .await
                                    }
                                    false => {
                                        agenda
                                            .update_status(&(r.command_id as usize), CommandStatus::Failure.into())
                                            .await
                                    }
                                },
                                Some(Response::Cancelled(c)) => {
                                    agenda
                                        .update_status(
                                            &(c.command_id as usize),
                                            CommandStatus::Cancelled(c.result).into(),
                                        )
                                        .await;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[async_trait]
impl PlatformDescriptor for Platform {
    ///Launch the platform (such as the simulation in godot) and open communication
    async fn start(&self) {
        self.inner.write().await.start().await;

        let server: SocketAddr = self.socket().await;
        //println!("server addr: {}", server);
        let client: PlatformInterfaceClient<_> =
            PlatformInterfaceClient::connect(format!("https://{}", server))
                .await
                .expect("error to connecting to client");
        let client2 = client.clone();
        let state = self.state.clone();
        tokio::spawn(async move {
            Platform::get_updates(client, state).await;
        });

        let agenda = self.agenda.clone();
        let (tx, command_stream) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);
        *self.command_stream.lock().await = Some(tx);
        tokio::spawn(async move {
            Platform::send_commands(client2, agenda, command_stream).await;
        });
    }

    ///Stops the platform.
    async fn stop(&self) {
        self.inner.write().await.stop().await;
    }

    ///Returns the domain of the platform
    async fn domain(&self) -> Domain {
        self.inner.read().await.domain().await
    }

    ///Returns a module loaded into the evaluation environment with other bindings
    async fn module(&self) -> Option<PlatformModule> {
        self.inner.read().await.module().await
    }

    ///Returns the server info in order to connect OMPAS to the platform using grpc services
    async fn socket(&self) -> SocketAddr {
        self.inner.read().await.socket().await
    }
}

#[derive(Clone)]
pub enum Domain {
    String(String),
    File(PathBuf),
}

impl Default for Domain {
    fn default() -> Self {
        Self::String("()".to_string())
    }
}

impl From<String> for Domain {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<PathBuf> for Domain {
    fn from(p: PathBuf) -> Self {
        Self::File(p)
    }
}

pub struct PlatformModule {
    module: Module,
    documentation: Documentation,
    pure_fonctions: PureFonctionCollection,
}

impl PlatformModule {
    pub fn new(module: impl IntoModule) -> Self {
        let documentation = module.documentation();
        let pure_fonctions = module.pure_fonctions();
        let module = module.into_module();
        Self {
            module,
            documentation,
            pure_fonctions,
        }
    }
}

impl IntoModule for PlatformModule {
    fn into_module(self) -> Module {
        self.module
    }

    fn documentation(&self) -> Documentation {
        self.documentation.clone()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        self.pure_fonctions.clone()
    }
}

/// Trait that a platform needs to implement to be able to be used as execution platform in RAE.
#[async_trait]
pub trait PlatformDescriptor: Any + Send + Sync {
    ///Launch the platform (such as the simulation in godot) and open communication
    async fn start(&self);

    ///Stops the platform.
    async fn stop(&self);

    ///Returns the domain of the platform
    async fn domain(&self) -> Domain;

    ///Returns a module loaded into the evaluation environment with other bindings
    async fn module(&self) -> Option<PlatformModule>;

    ///Returns the server info in order to connect OMPAS to the platform using grpc services
    async fn socket(&self) -> SocketAddr;
}
