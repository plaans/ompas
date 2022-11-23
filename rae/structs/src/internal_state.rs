use crate::agenda::Agenda;
use crate::monitor::MonitorCollection;
use crate::rae_command::RAECommand;
use crate::resource::ResourceCollection;
use crate::state::world_state::WorldState;
use ompas_middleware::ompas_log::TopicId;
use std::sync::Arc;
use tokio::sync::mpsc::Sender;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct OMPASInternalState {
    pub state: WorldState,
    pub resources: ResourceCollection,
    pub monitors: MonitorCollection,
    pub agenda: Agenda,
    pub log: TopicId,
    pub command_stream: Arc<RwLock<Option<Sender<RAECommand>>>>,
    //pub stop_tx: Arc<RwLock<Option<Sender<EndSignal>>>>,
    //pub process_interface: Arc<RwLock<Option<ProcessInterface>>>,
}

/*#[derive(Default, Clone)]
pub struct LogConfig {
    pub path: PathBuf,
    pub channel: Option<Sender<String>>,
    pub display: bool,
}*/

impl OMPASInternalState {
    pub async fn get_sender(&self) -> Option<Sender<RAECommand>> {
        self.command_stream.read().await.clone()
    }

    /*pub async fn get_stop(&self) -> Option<Sender<EndSignal>> {
        self.stop_tx.read().await.clone()
    }*/

    /*pub async fn get_killer(&self) -> Option<broadcast::Sender<EndSignal>> {
        self.killer.read().await.clone()
    }

    pub async fn subscribe_to_killer(&self) -> broadcast::Receiver<EndSignal> {
        self.killer.read().await.as_ref().unwrap().subscribe()
    }*/
}
