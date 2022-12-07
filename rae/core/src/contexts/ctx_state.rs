use ompas_rae_structs::state::world_state::WorldState;

pub struct CtxState {
    pub state: WorldState,
}

impl CtxState {
    pub fn new(state: WorldState) -> Self {
        Self { state }
    }
}
