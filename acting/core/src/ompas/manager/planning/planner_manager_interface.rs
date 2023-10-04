use crate::ompas::manager::planning::planner_stat::PlannerStat;
use crate::ompas::manager::planning::problem_update::PlannerUpdate;
use crate::TOKIO_CHANNEL_SIZE;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, RwLock};

pub struct PlannerManagerInterface {
    planner_update_sender: mpsc::UnboundedSender<PlannerUpdate>,
    watcher: (broadcast::Sender<bool>, broadcast::Receiver<bool>),
    stat: Arc<RwLock<PlannerStat>>,
}

impl PlannerManagerInterface {
    pub fn new(
        planner_update_sender: mpsc::UnboundedSender<PlannerUpdate>,
        stat: Arc<RwLock<PlannerStat>>,
    ) -> Self {
        Self {
            planner_update_sender,
            watcher: broadcast::channel(TOKIO_CHANNEL_SIZE),
            stat,
        }
    }

    pub fn send_update(&self, pu: PlannerUpdate) {
        let _ = self.planner_update_sender.send(pu);
    }

    pub fn notify_update_tree(&self) {
        self.watcher.0.send(true).unwrap_or_else(|_| panic!());
    }

    pub fn subscribe_on_update(&self) -> broadcast::Receiver<bool> {
        self.watcher.0.subscribe()
    }

    pub async fn get_stat(&self) -> PlannerStat {
        self.stat.read().await.clone()
    }
}
