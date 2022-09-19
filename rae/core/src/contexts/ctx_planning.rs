use ompas_rae_planning::aries::structs::ConversionCollection;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::select_mode::SelectMode;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lenv::LEnv;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;

pub const CTX_PLANNING: &str = "CtxPlanning";

pub struct CtxPlanning {
    pub env: LEnv,
    pub domain: RAEDomain,
    pub cc: Option<ConversionCollection>,
    pub select_mode: SelectMode,
}

impl CtxPlanning {
    pub fn new(
        cc: Option<ConversionCollection>,
        domain: RAEDomain,
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
            label: CTX_PLANNING.to_string(),
        }
    }

    fn documentation(&self) -> Documentation {
        vec![].into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![].into()
    }
}
