use ompas_language::exec::mode::CTX_RAE_MODE;
use sompas_structs::contextcollection::Context;

#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub enum RAEMode {
    #[default]
    Exec,
    Simu,
}

impl From<RAEMode> for Context {
    fn from(c: RAEMode) -> Self {
        Context::new(c, CTX_RAE_MODE)
    }
}
