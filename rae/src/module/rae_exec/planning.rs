use crate::context::rae_env::DomainEnv;
use crate::supervisor::options::SelectMode;
use ompas_lisp::core::structs::contextcollection::Context;
use ompas_lisp::core::structs::documentation::Documentation;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::module::{IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;

pub const MOD_PLANNING: &str = "mod-planning";
pub struct CtxPlanning {
    pub env: LEnv,
    pub domain: DomainEnv,
    pub select_mode: SelectMode,
}

impl CtxPlanning {
    pub fn new(domain: DomainEnv, env: LEnv, select_mode: SelectMode) -> Self {
        Self {
            domain,
            env,
            select_mode,
        }
    }
}

impl IntoModule for CtxPlanning {
    fn into_module(self) -> Module {
        Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_PLANNING.to_string(),
        }
    }

    fn documentation(&self) -> Documentation {
        vec![].into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![].into()
    }
}
