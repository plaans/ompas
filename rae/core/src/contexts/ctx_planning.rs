use ompas_rae_planning::aries::structs::ConversionCollection;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::select_mode::SelectMode;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::DocCollection;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::purefonction::PureFonctionCollection;

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
