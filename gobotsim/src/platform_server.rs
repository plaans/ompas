use crate::TOKIO_CHANNEL_SIZE;
use async_trait::async_trait;
use ompas_rae_interface::platform_interface::platform_interface_server::PlatformInterface;
use ompas_rae_interface::platform_interface::platform_update::Update;
use ompas_rae_interface::platform_interface::{
    CommandRequest, CommandResponse, InitGetUpdate, PlatformUpdate, StateUpdate,
};
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

#[async_trait]
impl PlatformInterface for PlatformGobotSimService {
    type GetUpdatesStream = tokio_stream::wrappers::ReceiverStream<Result<PlatformUpdate, Status>>;

    async fn get_updates(
        &self,
        request: Request<InitGetUpdate>,
    ) -> Result<Response<Self::GetUpdatesStream>, Status> {
        let (tx, rx) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);

        println!("[GobotSimService] received request for updates!");
        let request: InitGetUpdate = request.into_inner();

        let mut state_update = self.state_update.resubscribe();

        tokio::spawn(async move {
            while let Ok(msg) = state_update.recv().await {
                tx.send(Ok(msg)).await;
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
        println!("[GobotSimService] received request to execute stream of commands!");
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        //Two threads, one handling the sending of request, the other one handling the reponses.
        let mut command_request_to_godot = self.command_request.clone();
        let mut command_request_receiver = request.into_inner();

        tokio::spawn(async move {
            while let Ok(Some(request)) = command_request_receiver.message().await {
                command_request_to_godot.send(request).await;
            }
        });

        let mut command_response_receiver = self.command_response.resubscribe();

        tokio::spawn(async move {
            while let Ok(response) = command_response_receiver.recv().await {
                tx.send(Ok(response)).await;
            }
        });

        Ok(Response::new(ReceiverStream::new(rx)))
    }
}
