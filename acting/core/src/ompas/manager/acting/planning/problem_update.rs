use crate::ompas::manager::acting::acting_var::ActingVarId;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId};
use crate::ompas::manager::state::world_state::WorldStateSnapshot;
use crate::planning::planner::problem::ChronicleInstance;
use crate::TOKIO_CHANNEL_SIZE;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};

const PROBLEM_UPDATE_RUN: &str = "problem_update_run";

pub enum ProblemUpdate {
    Instanciation(Instanciation),
    ExecutionProblem(ExecutionProblem),
    UpdateState(UpdateState),
}

pub struct ExecutionProblem {
    pub(crate) state: WorldStateSnapshot,
    pub(crate) chronicles: Vec<ChronicleInstance>,
}

pub struct Instanciation {}

pub struct UpdateState {}

pub enum ActingUpdateNotification {
    VarUpdate(ActingVarId),
    NewProcess(ActingProcessId),
}

pub struct ProblemUpdateManager {
    acting_manager: ActingManager,
    notifier: mpsc::Receiver<ActingUpdateNotification>,
    updater: mpsc::Sender<ProblemUpdate>,
}

impl ProblemUpdateManager {
    pub fn new(
        acting_manager: ActingManager,
    ) -> (
        Self,
        Receiver<ProblemUpdate>,
        Sender<ActingUpdateNotification>,
    ) {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (tx_notif, rx_notif) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        (
            Self {
                acting_manager,
                notifier: rx_notif,
                updater: tx,
            },
            rx,
            tx_notif,
        )
    }

    pub async fn run(self) {
        let mut process_interface: ProcessInterface =
            ProcessInterface::new(PROBLEM_UPDATE_RUN, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

        let ProblemUpdateManager {
            acting_manager,
            mut notifier,
            updater,
        } = self;

        'main: loop {
            tokio::select! {
                _ = process_interface.recv() => {
                    break 'main;
                }
                Some(notification) = notifier.recv() => {
                    let notification: ActingUpdateNotification = notification;
                    match notification {
                        ActingUpdateNotification::VarUpdate(_) => {}
                        ActingUpdateNotification::NewProcess(_) => {
                             let em: ExecutionProblem = acting_manager.get_execution_problem().await;
                            /*for i in &em.chronicles {
                                println!("{}", i.am.chronicle.as_ref().unwrap())
                            }*/
                            //exit(0);
                            updater.send(ProblemUpdate::ExecutionProblem(em)).await.unwrap_or_else(|_| panic!(""));
                        }
                    }


                    //Construction of the instances
                    // In the current implementation we do not take into account the local modifications,

                    // and thus send a whole new problem containing the new chronicles and the instanciations.
                    //Sends the new problem to the planner
                }
            }
        }
    }
}
