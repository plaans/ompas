use crate::ompas::manager::acting::ActingProcessId;
use crate::ompas::manager::planning::planner_stat::PlannerStat;
use crate::ompas::manager::planning::problem_update::PlannerUpdate;
use aries::collections::seq::Seq;
use std::collections::HashSet;
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};

pub struct PlannerWatcher {
    sender: mpsc::UnboundedSender<Vec<ActingProcessId>>,
    watched_processes: FilterWatchedProcesses,
}

pub enum FilterWatchedProcesses {
    All,
    Some(Vec<ActingProcessId>),
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

    pub fn notify_update_tree(&mut self, updated: FilterWatchedProcesses) {
        let mut too_remove = vec![];
        match updated {
            FilterWatchedProcesses::All => {
                for (
                    id,
                    PlannerWatcher {
                        sender,
                        watched_processes: _,
                    },
                ) in self.watchers.iter().enumerate()
                {
                    if sender.send(vec![]).is_err() {
                        too_remove.push(id)
                    }
                }
            }
            FilterWatchedProcesses::Some(updated) => {
                let updated = updated.to_set();
                for (
                    id,
                    PlannerWatcher {
                        sender,
                        watched_processes,
                    },
                ) in self.watchers.iter().enumerate()
                {
                    let watched: Vec<_> = match watched_processes {
                        FilterWatchedProcesses::All => updated.clone().to_vec(),
                        FilterWatchedProcesses::Some(watched_processes) => {
                            let watched_set: HashSet<_> = watched_processes.iter().collect();
                            watched_set
                                .intersection(&updated.iter().collect())
                                .map(|id| **id)
                                .collect()
                        }
                    };

                    if !watched.is_empty() && sender.send(watched).is_err() {
                        too_remove.push(id)
                    }
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
        watched_processes: FilterWatchedProcesses,
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
