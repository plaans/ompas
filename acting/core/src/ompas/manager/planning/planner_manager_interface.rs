use crate::ompas::manager::planning::problem_update::PlannerUpdate;
use crate::TOKIO_CHANNEL_SIZE;
use tokio::sync::{broadcast, mpsc};

pub struct PlannerManagerInterface {
    planner_update_sender: mpsc::Sender<PlannerUpdate>,
    watcher: (broadcast::Sender<bool>, broadcast::Receiver<bool>),
}

impl PlannerManagerInterface {
    pub fn new(planner_update_sender: mpsc::Sender<PlannerUpdate>) -> Self {
        Self {
            planner_update_sender,
            watcher: broadcast::channel(TOKIO_CHANNEL_SIZE),
        }
    }

    pub async fn send_update(&self, pu: PlannerUpdate) {
        let _ = self.planner_update_sender.send(pu).await;
    }

    pub fn notify_update_tree(&self) {
        self.watcher.0.send(true).unwrap_or_else(|_| panic!());
    }

    pub fn subscribe_on_update(&self) -> broadcast::Receiver<bool> {
        self.watcher.0.subscribe()
    }
}
