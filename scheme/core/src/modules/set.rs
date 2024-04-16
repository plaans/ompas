use crate::modules::list::{get_list, set_list};
use crate::modules::map::{get_map, set_map};
use sompas_language::set::*;
use sompas_macros::scheme_fn;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::wrong_n_args;

#[derive(Default)]
pub struct ModSet {}

impl From<ModSet> for LModule {
    fn from(m: ModSet) -> LModule {
        let mut module = LModule::new(m, MOD_SET, DOC_MOD_SET);
        module.add_fn(SET, set, DOC_SET, true);
        module.add_fn(GET, get, DOC_GET, true);
        module.add_fn(IS_EMPTY, empty, DOC_IS_EMPTY, true);
        module.add_fn(LEN, len, DOC_LEN, true);
        module
    }
}

#[scheme_fn]
pub fn set(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            SET,
            args,
            1..std::usize::MAX,
        ));
    }
    let first = &args[0];
    match first {
        LValue::Map(_) => set_map(env, args),
        LValue::List(_) | LValue::Nil => set_list(env, args),
        _ => Err(LRuntimeError::not_in_list_of_expected_types(
            SET,
            first,
            vec![KindLValue::List, KindLValue::Map, KindLValue::Nil],
        )),
    }
}

#[scheme_fn]
pub fn get(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(wrong_n_args!(GET, args, 2));
    }
    match &args[0] {
        LValue::Map(_) => get_map(env, args),
        LValue::List(_) | LValue::Nil => get_list(env, args),
        _ => Err(LRuntimeError::not_in_list_of_expected_types(
            GET,
            &args[0],
            vec![KindLValue::List, KindLValue::Map, KindLValue::Nil],
        )),
    }
}

/// Returns true if a hashmap or list is empty
#[scheme_fn]
pub fn empty(lv: &LValue) -> Result<bool, LRuntimeError> {
    match lv {
        LValue::List(l) => Ok(l.is_empty()),
        LValue::Map(m) => Ok(m.is_empty()),
        LValue::Nil => Ok(true),
        lv => Err(LRuntimeError::not_in_list_of_expected_types(
            IS_EMPTY,
            lv,
            vec![KindLValue::List, KindLValue::Map, KindLValue::Nil],
        )),
    }
}

/// return the length of the object if it is a table or a list.
#[scheme_fn]
pub fn len(lv: &LValue) -> Result<usize, LRuntimeError> {
    match lv {
        LValue::List(l) => Ok(l.len()),
        LValue::Map(m) => Ok(m.len()),
        LValue::Nil => Ok(0),
        lv => Err(LRuntimeError::not_in_list_of_expected_types(
            LEN,
            lv,
            vec![KindLValue::List, KindLValue::Map],
        )),
    }
}
