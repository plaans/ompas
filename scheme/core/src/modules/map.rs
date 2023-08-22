use im::HashMap;
use sompas_language::map::*;
use sompas_macros::scheme_fn;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_n_args, wrong_type};

#[derive(Default)]
pub struct ModMap {}

impl From<ModMap> for LModule {
    fn from(m: ModMap) -> Self {
        let mut module = LModule::new(m, MOD_MAP, DOC_MOD_MAP);
        module.add_fn(FN_MAP, fn_map, (DOC_FN_MAP, DOC_FN_MAP_VERBOSE), true);
        module.add_fn(GET_MAP, get_map, (DOC_GET_MAP, DOC_GET_MAP_VERBOSE), true);
        module.add_fn(SET_MAP, set_map, (DOC_SET_MAP, DOC_SET_MAP_VERBOSE), true);
        module.add_fn(REMOVE_MAP, remove_map, DOC_REMOVE_MAP, true);
        module.add_fn(
            REMOVE_KEY_VALUE_MAP,
            remove_key_value_map,
            DOC_REMOVE_KEY_VALUE_MAP,
            true,
        );
        module.add_fn(UNION_MAP, union_map, DOC_UNION_MAP, true);
        module.add_fn(
            MAP_AS_LIST,
            map_as_list,
            (DOC_MAP_AS_LIST, DOC_MAP_AS_LIST_VERBOSE),
            true,
        );

        module
    }
}

#[scheme_fn]
pub fn fn_map(list: Vec<LValue>) -> Result<im::HashMap<LValue, LValue>, LRuntimeError> {
    let mut facts: HashMap<LValue, LValue> = Default::default();
    for sv in &list {
        match sv {
            LValue::List(val_sv) => {
                if val_sv.len() != 2 {
                    return Err(wrong_n_args!(FN_MAP, val_sv, 2));
                }
                let key = val_sv[0].clone();
                let value = val_sv[1].clone();
                facts.insert(key, value);
            }
            lv => return Err(wrong_type!(FN_MAP, lv, KindLValue::List)),
        }
    }
    Ok(facts)
}

#[scheme_fn]
pub fn get_map(map: im::HashMap<LValue, LValue>, key: &LValue) -> LValue {
    map.get(key).unwrap_or(&LValue::Nil).clone()
}
#[scheme_fn]
pub fn set_map(
    map: im::HashMap<LValue, LValue>,
    val: Vec<LValue>,
) -> Result<im::HashMap<LValue, LValue>, LRuntimeError> {
    if val.len() == 2 {
        let key = val[0].clone();
        let value = val[1].clone();
        Ok(map.update(key, value))
    } else {
        Err(wrong_n_args!(SET_MAP, &val, 2))
    }
}

#[scheme_fn]
pub fn remove_map(
    mut map: im::HashMap<LValue, LValue>,
    key: &LValue,
) -> Result<HashMap<LValue, LValue>, LRuntimeError> {
    map.remove(key);
    Ok(map)
}

#[scheme_fn]
pub fn remove_key_value_map(
    mut map: im::HashMap<LValue, LValue>,
    val: Vec<LValue>,
) -> Result<HashMap<LValue, LValue>, LRuntimeError> {
    if val.len() != 2 {
        return Err(lruntimeerror!(
            REMOVE_KEY_VALUE_MAP,
            format!("{} is supposed to be a pair (key value)", LValue::from(val))
        ));
    }

    let key = &val[0];
    let value = &val[1];
    match map.get(key) {
        None => Err(lruntimeerror!(
            REMOVE_KEY_VALUE_MAP,
            format!("map does not contain key {}", key)
        )),
        Some(v) => {
            if v == value {
                map.remove(key);
                Ok(map)
            } else {
                Err(lruntimeerror!(
                    REMOVE_KEY_VALUE_MAP,
                    format!("map does not have key value ({} {})", key, value)
                ))
            }
        }
    }
}

/// Merges two hashmap tables
#[scheme_fn]
pub fn union_map(
    map1: im::HashMap<LValue, LValue>,
    map2: im::HashMap<LValue, LValue>,
) -> HashMap<LValue, LValue> {
    map1.union(map2)
}

#[scheme_fn]
pub fn map_as_list(map: im::HashMap<LValue, LValue>) -> Vec<LValue> {
    map.iter()
        .map(|(k, v)| list!(k.clone(), v.clone()))
        .collect()
}
