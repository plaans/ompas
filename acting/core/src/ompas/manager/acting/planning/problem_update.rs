use crate::ompas::manager::state::world_state::WorldStateSnapshot;
use crate::planning::planner::problem::ChronicleInstance;
use tokio::sync::mpsc;
use tokio::sync::mpsc::Receiver;

const BUFFER_SIZE: usize = 100;

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

pub struct ProblemUpdateManager {
    channel: mpsc::Sender<ProblemUpdate>,
}

impl ProblemUpdateManager {
    pub fn new() -> (Self, Receiver<ProblemUpdate>) {
        let (tx, rx) = mpsc::channel(BUFFER_SIZE);
        (Self { channel: tx }, rx)
    }

    pub async fn run(&mut self) {
        todo!()
    }
}
