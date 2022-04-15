use ompas_rae_structs::exec_context::options::SelectMode;
use ompas_rae_structs::exec_context::rae_env::DomainEnv;
use ompas_rae_structs::planning::ConversionCollection;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lenv::LEnv;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;

pub const MOD_PLANNING: &str = "mod-planning";
pub struct CtxPlanning {
    pub env: LEnv,
    pub domain: DomainEnv,
    pub cc: Option<ConversionCollection>,
    pub select_mode: SelectMode,
}

impl CtxPlanning {
    pub fn new(
        cc: Option<ConversionCollection>,
        domain: DomainEnv,
        env: LEnv,
        select_mode: SelectMode,
    ) -> Self {
        Self {
            domain,
            env,
            select_mode,
            cc,
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
