use ompas_language::exec::mode::CTX_MODE;
use sompas_structs::contextcollection::Context;

#[derive(Copy, Clone)]
pub enum RAEMode {
    Exec,
    Simu,
}

pub struct CtxMode {
    pub mode: RAEMode,
}

impl From<CtxMode> for Context {
    fn from(c: CtxMode) -> Self {
        Context::new(c, CTX_MODE)
    }
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
