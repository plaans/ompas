use crate::agenda::Agenda;
use crate::monitor::MonitorCollection;
use crate::platform::Log;
use crate::rae_command::RAECommand;
use crate::resource::ResourceCollection;
use crate::state::world_state::WorldState;
use sompas_utils::task_handler::EndSignal;
use std::sync::Arc;
use tokio::sync::mpsc::Sender;
use tokio::sync::{broadcast, RwLock};

#[derive(Default, Clone)]
pub struct RAEInterface {
    pub state: WorldState,
    pub resources: ResourceCollection,
    pub monitors: MonitorCollection,
    pub agenda: Agenda,
    pub log: Log,
    pub command_tx: Arc<RwLock<Option<Sender<RAECommand>>>>,
    //pub stop_tx: Arc<RwLock<Option<Sender<EndSignal>>>>,
    pub killer: Arc<RwLock<Option<broadcast::Sender<EndSignal>>>>,
}

impl RAEInterface {
    pub async fn get_sender(&self) -> Option<Sender<RAECommand>> {
        self.command_tx.read().await.clone()
    }

    /*pub async fn get_stop(&self) -> Option<Sender<EndSignal>> {
        self.stop_tx.read().await.clone()
    }*/

    pub async fn get_killer(&self) -> Option<broadcast::Sender<EndSignal>> {
        self.killer.read().await.clone()
    }

    pub async fn subscribe_to_killer(&self) -> broadcast::Receiver<EndSignal> {
        self.killer.read().await.as_ref().unwrap().subscribe()
    }
}
