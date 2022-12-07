#[derive(Copy, Clone)]
pub enum RAEMode {
    Exec,
    Simu,
}

pub struct CtxMode {
    pub mode: RAEMode,
}

impl CtxMode {
    pub fn new(mode: RAEMode) -> Self {
        Self { mode }
    }
}

impl Default for CtxMode {
    fn default() -> Self {
        Self {
            mode: RAEMode::Exec,
        }
    }
}
