use crate::ompas::manager::acting::ActingProcessId;
use crate::ompas::manager::planning::planner_stat::PlannerStat;
use crate::ompas::manager::planning::problem_update::PlannerUpdate;
use aries::collections::seq::Seq;
use std::collections::HashSet;
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};

pub struct PlannerWatcher {
    sender: mpsc::UnboundedSender<Vec<ActingProcessId>>,
    watched_processes: Vec<ActingProcessId>,
}

pub struct PlannerManagerInterface {
    planner_update_sender: mpsc::UnboundedSender<PlannerUpdate>,
    watchers: Vec<PlannerWatcher>,
    stat: Arc<RwLock<PlannerStat>>,
}

impl PlannerManagerInterface {
    pub fn new(
        planner_update_sender: mpsc::UnboundedSender<PlannerUpdate>,
        stat: Arc<RwLock<PlannerStat>>,
    ) -> Self {
        Self {
            planner_update_sender,
            watchers: vec![],
            stat,
        }
    }

    pub fn send_update(&self, pu: PlannerUpdate) {
        let _ = self.planner_update_sender.send(pu);
    }

    pub fn notify_update_tree(&mut self, updated: Vec<ActingProcessId>) {
        let mut too_remove = vec![];
        let updated = updated.to_set();
        for (
            id,
            PlannerWatcher {
                sender,
                watched_processes,
            },
        ) in self.watchers.iter().enumerate()
        {
            let watched_set: HashSet<_> = watched_processes.iter().collect();
            let inter: Vec<_> = watched_set
                .intersection(&updated.iter().collect())
                .map(|id| **id)
                .collect();
            if !inter.is_empty() {
                if let Err(_) = sender.send(inter) {
                    too_remove.push(id)
                }
            }
        }

        too_remove.reverse();
        for id in too_remove {
            self.watchers.remove(id);
        }
    }

    pub fn subscribe_on_update(
        &mut self,
        watched_processes: Vec<ActingProcessId>,
    ) -> mpsc::UnboundedReceiver<Vec<ActingProcessId>> {
        let (tx, rx) = mpsc::unbounded_channel();
        self.watchers.push(PlannerWatcher {
            sender: tx,
            watched_processes,
        });
        rx
    }

    pub async fn get_stat(&self) -> PlannerStat {
        self.stat.read().await.clone()
    }
}
