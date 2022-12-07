use sompas_language::sort::{DOC_MOD_SORT, MOD_SORT};
use sompas_structs::lmodule::LModule;

pub struct ModSort {}

impl From<ModSort> for LModule {
    fn from(m: ModSort) -> Self {
        LModule::new(m, MOD_SORT, DOC_MOD_SORT)
    }
}
