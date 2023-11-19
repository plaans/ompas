use crate::advanced_math::ModAdvancedMath;
use crate::first_order_logic::ModFirstOrderLogic;
use crate::io::{LogOutput, ModIO};
use crate::sort::ModSort;
use crate::string::ModString;
use crate::time::ModTime;
use crate::utils::ModUtils;
use sompas_language::extended_std::{DOC_MOD_EXTENDED_STD, MOD_EXTENDED_STD};
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lmodule::LModule;

pub mod advanced_math;
pub mod first_order_logic;
pub mod io;
pub mod sort;
pub mod string;
pub mod time;
pub mod utils;

pub struct ModExtendedStd {
    log_output: LogOutput,
}

impl Default for ModExtendedStd {
    fn default() -> Self {
        Self {
            log_output: LogOutput::Stdout,
        }
    }
}

impl ModExtendedStd {
    pub fn set_log_output(&mut self, log_output: LogOutput) {
        self.log_output = log_output;
    }
}

impl From<ModExtendedStd> for LModule {
    /// Returns all basic functions, macros, and lambdas
    ///
    fn from(m: ModExtendedStd) -> LModule {
        let log_output = m.log_output.clone();
        let mut module = LModule::new(m, MOD_EXTENDED_STD, DOC_MOD_EXTENDED_STD);
        module.add_submodule(ModAdvancedMath::default(), WithoutPrefix);
        module.add_submodule(ModSort::default(), WithoutPrefix);
        module.add_submodule(ModString::default(), WithoutPrefix);
        module.add_submodule(ModTime::new(), WithoutPrefix);
        module.add_submodule(ModUtils::default(), WithoutPrefix);
        module.add_submodule(ModFirstOrderLogic::default(), WithoutPrefix);
        module.add_submodule(ModIO::new(log_output), WithoutPrefix);

        module
    }
}
