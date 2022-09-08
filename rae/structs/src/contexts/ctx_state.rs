use crate::state::world_state::WorldState;

pub const CTX_STATE: &str = "CtxState";

pub struct CtxState {
    pub state: WorldState,
}
