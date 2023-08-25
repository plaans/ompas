use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::ompas::manager::acting::acting_var::PlanVarRef;
use crate::ompas::manager::acting::ActingProcessId;
use crate::ompas::manager::state::state_manager::WorldStateSnapshot;
use crate::planning::planner::problem::ChronicleInstance;

pub enum PlannerUpdate {
    Plan,
    VarUpdate(VarUpdate),
    ProblemUpdate(ActingProcessId),
    StateUpdate(StateUpdate),
}

pub struct ExecutionProblem {
    pub(crate) state: WorldStateSnapshot,
    pub(crate) chronicles: Vec<ChronicleInstance>,
}

#[derive(Debug, Clone)]
pub struct VarUpdate {
    pub var_ref: PlanVarRef,
    pub value: Cst,
}

impl FormatWithSymTable for VarUpdate {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{{ var_id = {}, val = {}}}",
            self.var_ref.var_id().format(st, sym_version),
            self.value
        )
    }
}

pub struct StateUpdate {}

/*impl ProblemUpdateManager {
    pub fn new(
        acting_manager: ActingManager,
    ) -> (
        Self,
        Receiver<PlannerUpdate>,
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
        let mut index = 0;
        'main: loop {
            tokio::select! {
                _ = process_interface.recv() => {
                    break 'main;
                }
                Some(notification) = notifier.recv() => {
                    println!("Planning n°{index}");
                    let notification: ActingUpdateNotification = notification;
                    match notification {
                        ActingUpdateNotification::VarUpdate(v) => {
                            println!("Update of vars {:?}.", v);
                        }
                        ActingUpdateNotification::NewProcess(p) => {
                            println!("Planning with new process {p}.");
                        }
                        ActingUpdateNotification::Plan => {
                            println!("Requested replanning of tree.");
                        }
                    }
                    let em: ExecutionProblem = acting_manager.get_execution_problem().await;
                    updater.send(PlannerUpdate::ExecutionProblem(em)).await.unwrap_or_else(|_| panic!(""));
                    index+=1;


                    //Construction of the instances
                    // In the current implementation we do not take into account the local modifications,

                    // and thus send a whole new problem containing the new chronicles and the instanciations.
                    //Sends the new problem to the planner
                }
            }
        }
    }
}*/
