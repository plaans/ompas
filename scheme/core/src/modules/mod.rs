pub mod basic_math;
pub mod env;
pub mod error;
pub mod list;
pub mod map;
pub mod predicate;
mod primitive;
pub mod set;

use crate::modules::basic_math::ModBasicMath;
use crate::modules::env::*;
use crate::modules::error::ModError;
use crate::modules::list::ModList;
use crate::modules::map::ModMap;
use crate::modules::predicate::ModPredicate;
use crate::modules::primitive::ModPrimitive;
use crate::modules::set::ModSet;
use sompas_language::{DOC_MOD_STD, MOD_STD};
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;

#[derive(Default)]
pub struct ModRoot {}

impl From<ModRoot> for LModule {
    /// Returns all basic functions, macros, and lambdas
    ///
    fn from(m: ModRoot) -> LModule {
        let mut module = LModule::new(m, MOD_STD, DOC_MOD_STD);
        module.add_submodule(ModBasicMath::default());
        module.add_submodule(ModError::default());
        module.add_submodule(ModEnv::default());
        module.add_submodule(ModList::default());
        module.add_submodule(ModMap::default());
        module.add_submodule(ModPredicate::default());
        module.add_submodule(ModSet::default());
        module.add_submodule(ModPrimitive::default());

        module
    }
}
/// Default function of the Lisp Environement.
/// Does nothing outside returning a string.
#[scheme_fn]
pub fn default() -> String {
    "default function".to_string()
}
