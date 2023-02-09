use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::state::world_state::WorldStateSnapshot;
use aries_planning::chronicles::ChronicleOrigin;
use sompas_structs::lvalues::LValueS;

pub struct PlanningInstance {
    pub state: WorldStateSnapshot,
    pub tasks: Vec<LValueS>,
    pub instances: Vec<ChronicleInstance>,
}

pub struct ChronicleInstance {
    pub origin: ChronicleOrigin,
    pub template: ChronicleTemplate,
}
