use crate::{PROCESS_TOPIC_GOBOT_SIM, TOKIO_CHANNEL_SIZE};
use async_trait::async_trait;
use ompas_middleware::{LogLevel, ProcessInterface};
use ompas_rae_interface::platform_interface::platform_interface_server::PlatformInterface;
use ompas_rae_interface::platform_interface::{
    CommandRequest, CommandResponse, InitGetUpdate, PlatformUpdate,
};
use ompas_rae_language::interface::*;
use tokio::sync::{broadcast, mpsc};
use tokio_stream::wrappers::ReceiverStream;
use tonic::Response;
use tonic::Status;
use tonic::{Request, Streaming};

pub struct PlatformGobotSimService {
    pub command_request: mpsc::Sender<CommandRequest>,
    pub command_response: broadcast::Receiver<CommandResponse>,
    pub state_update: broadcast::Receiver<PlatformUpdate>,
}

const PROCESS_GOBOT_SIM_SERVICE_GET_UPDATES: &str = "__PROCESS_GOBOT_SIM_SERVICE_GET_UPDATES__";
const PROCESS_GOBOT_SIM_SERVICE_SEND_COMMANDS: &str = "__PROCESS_GOBOT_SIM_SERVICE_SEND_COMMANDS__";

#[async_trait]
impl PlatformInterface for PlatformGobotSimService {
    type GetUpdatesStream = tokio_stream::wrappers::ReceiverStream<Result<PlatformUpdate, Status>>;

    async fn get_updates(
        &self,
        _: Request<InitGetUpdate>,
    ) -> Result<Response<Self::GetUpdatesStream>, Status> {
        let mut process = ProcessInterface::new(
            PROCESS_GOBOT_SIM_SERVICE_GET_UPDATES,
            PROCESS_TOPIC_GOBOT_SIM,
            LOG_TOPIC_PLATFORM,
        )
        .await;
        process.log_info("Received request for updates!").await;
        let (tx, rx) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);
        //let request: InitGetUpdate = request.into_inner();

        let mut state_update = self.state_update.resubscribe();

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    _ = process.recv() => {
                        break; //process.die();
                    }
                    msg = state_update.recv() => {
                        if let Ok(msg) = msg {
                            if tx.send(Ok(msg)).await.is_err() {
                                process.kill(PROCESS_TOPIC_PLATFORM).await;
                                break; //process.die();
                            }
                        }
                    }

                }
            }
        });

        Ok(tonic::Response::new(
            tokio_stream::wrappers::ReceiverStream::new(rx),
        ))
    }

    type SendCommandsStream =
        tokio_stream::wrappers::ReceiverStream<Result<CommandResponse, Status>>;

    async fn send_commands(
        &self,
        request: Request<Streaming<CommandRequest>>,
    ) -> Result<Response<Self::SendCommandsStream>, Status> {
        let mut process: ProcessInterface = ProcessInterface::new(
            PROCESS_GOBOT_SIM_SERVICE_SEND_COMMANDS,
            PROCESS_TOPIC_GOBOT_SIM,
            LOG_TOPIC_PLATFORM,
        )
        .await;
        process
            .log_debug("Received request to execute stream of commands!")
            .await;
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        //Two threads, one handling the sending of request, the other one handling the reponses.
        let command_request_to_godot = self.command_request.clone();
        let mut command_request_receiver = request.into_inner();

        let mut command_response_receiver = self.command_response.resubscribe();

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    _ = process.recv() => {
                        break; //process.die();
                    }
                    msg = command_request_receiver.message() => {
                        if let Ok(Some(request)) = msg {
                            process.log("Received new command request.", LogLevel::Debug).await;
                            if command_request_to_godot.send(request).await.is_err() {
                                process.kill(PROCESS_TOPIC_PLATFORM).await;
                                break; //process.die();
                            }
                        }
                    }
                    msg = command_response_receiver.recv() => {
                        if let Ok(response) = msg {
                            if tx.send(Ok(response)).await.is_err() {
                                process.kill(PROCESS_TOPIC_PLATFORM).await;
                                break;// process.die();
                            }
                        }
                    }
                }
            }
        });

        Ok(Response::new(ReceiverStream::new(rx)))
    }
}
