use crate::conversion::chronicle::Chronicle;
use crate::state::world_state::WorldStateSnapshot;
use crate::supervisor::process::process_ref::ProcessRef;
use aries_planning::chronicles::ChronicleOrigin;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;

pub struct PlanningInstance {
    pub state: WorldStateSnapshot,
    pub tasks: Vec<LValueS>,
    pub instances: Vec<ChronicleInstance>,
}

pub struct ChronicleInstance {
    pub origin: ChronicleOrigin,
    pub chronicle: Chronicle,
    pub om: LValue,
    pub pr: ProcessRef,
}
