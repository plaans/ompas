use crate::platform_interface::command_request::Request;
use crate::platform_interface::command_response::Response;
use crate::platform_interface::platform_interface_client::PlatformInterfaceClient;
use crate::platform_interface::{
    event, CommandCancelRequest, CommandExecutionRequest, CommandRequest, CommandResponse,
    Expression, InitGetUpdate, PlatformUpdate, StateVariableType,
};
use crate::{platform_interface, TOKIO_CHANNEL_SIZE};
use async_trait::async_trait;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::state::action_status::CommandStatus;
use ompas_rae_structs::state::partial_state::PartialState;
use ompas_rae_structs::state::world_state::{StateType, WorldState};
use platform_interface::platform_update::Update;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use sompas_structs::module::IntoModule;
use sompas_utils::task_handler::EndSignal;
use std::any::Any;
use std::borrow::Borrow;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::{broadcast, Mutex};

#[derive(Clone)]
pub struct Platform {
    pub inner: Arc<tokio::sync::RwLock<dyn PlatformDescriptor>>,
    pub state: WorldState,
    pub agenda: Agenda,
    pub command_stream: Arc<Mutex<Option<tokio::sync::mpsc::Sender<CommandRequest>>>>,
    pub killer: Arc<tokio::sync::RwLock<Option<tokio::sync::broadcast::Sender<EndSignal>>>>,
}

impl Platform {
    pub async fn exec_command(&self, command: &[LValue], command_id: usize) {
        let mut arguments: Vec<Expression> = vec![];
        for arg in command {
            arguments.push(LValueS::try_from(arg).unwrap().try_into().unwrap())
        }

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
            .await;
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
            .await;
    }

    async fn get_updates(
        mut client: PlatformInterfaceClient<tonic::transport::Channel>,
        world_state: WorldState,
        killer: tokio::sync::broadcast::Sender<EndSignal>,
    ) {
        let mut killed = killer.subscribe();
        let request = tonic::IntoRequest::into_request(InitGetUpdate {});
        println!("[PlatformClient] initiating update stream");
        let stream = client.get_updates(request).await.expect("");
        let mut stream: tonic::codec::Streaming<PlatformUpdate> = stream.into_inner();

        loop {
            tokio::select! {
                _ = killed.recv() => {
                    eprintln!("platform update service ended.");
                    break;
                }
                msg = stream.message() => {
                    //println!("[PlatformClient] received new update: {:?}", msg);
                    match msg {
                    Err(_) => {
                         killer.send(true).expect("could send kill message to rae processes.");
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
                    _ => unimplemented!()
                }
                }
            }
        }
    }

    async fn send_commands(
        mut client: PlatformInterfaceClient<tonic::transport::Channel>,
        agenda: Agenda,
        mut command_stream: tokio::sync::mpsc::Receiver<CommandRequest>,
        killer: tokio::sync::broadcast::Sender<EndSignal>,
    ) {
        /*let stream = async_stream::stream! {
            loop {
                let command = command_stream.recv().await.unwrap();
                yield command
            }
        };*/
        let stream = tokio_stream::wrappers::ReceiverStream::new(command_stream);
        println!("[PlatformClient] initiating command stream");

        let stream = client
            .send_commands(tonic::Request::new(stream))
            .await
            .expect("");
        let mut stream = stream.into_inner();

        while let Some(command_response) = stream.message().await.expect("") {
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

#[async_trait]
impl PlatformDescriptor for Platform {
    ///Launch the platform (such as the simulation in godot) and open communication
    async fn start(&self) {
        self.inner.write().await.start().await;

        let server: SocketAddr = self.socket().await;
        println!("server addr: {}", server);
        let client: PlatformInterfaceClient<_> =
            PlatformInterfaceClient::connect(format!("https://{}", server))
                .await
                .expect("error to connecting to client");
        let client2 = client.clone();
        let state = self.state.clone();
        let (killer, killed) = broadcast::channel(TOKIO_CHANNEL_SIZE);
        let killer2 = killer.clone();
        tokio::spawn(async move {
            Platform::get_updates(client, state, killer).await;
        });

        let agenda = self.agenda.clone();
        let (tx, command_stream) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);
        *self.command_stream.lock().await = Some(tx);
        tokio::spawn(async move {
            Platform::send_commands(client2, agenda, command_stream, killer2).await;
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
    module: Box<dyn IntoModule>,
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
