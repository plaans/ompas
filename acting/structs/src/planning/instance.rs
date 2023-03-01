use crate::acting_manager::operational_model::ActingModel;
use crate::acting_manager::process::process_ref::ProcessRef;
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
    pub om: ActingModel,
    pub pr: ProcessRef,
}
