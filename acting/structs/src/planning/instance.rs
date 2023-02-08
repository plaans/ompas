use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::state::world_state::WorldStateSnapshot;
use sompas_structs::lvalues::LValueS;

pub struct PlanningInstance {
    pub state: WorldStateSnapshot,
    pub tasks: Vec<LValueS>,
    pub instances: Vec<ChronicleTemplate>,
}
