use crate::core::language::MAP;
use crate::core::root_module::map::language::*;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::*;
use crate::core::structs::lerror::LResult;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;
use im::HashMap;

pub mod language {

    pub const GET_MAP: &str = "get-map";
    pub const SET_MAP: &str = "set-map";
    pub const REMOVE_MAP: &str = "remove-map";
    pub const REMOVE_KEY_VALUE_MAP: &str = "remove-key-value-map";
    pub const UNION_MAP: &str = "union-map";

    pub const DOC_MAP: &str = "Return a map from from a list of pairs.";
    pub const DOC_MAP_VERBOSE: &str = "Example: (map (quote ((ten . 10) (twenty . 20))))";
    pub const DOC_GET: &str = "Takes a key as argument and return the binding if defined in the environment. Return the key otherwise.";
    pub const DOC_GET_MAP: &str =
        "Takes a map and a key as argument, and return the value associated.";
    pub const DOC_GET_MAP_VERBOSE: &str = "Example: Here is an example in the repl\n\
                                        \t>> (define m (map (quote ((ten . 10) (twenty . 20)))))\n\
                                        \t>> (get-map m ten)\n\
                                        \tLI>> 10";
    pub const DOC_SET_MAP: &str = "Takes a map and and a list of pairs (key . value) to set in the map. Return a new map with the new bindings";
    pub const DOC_SET_MAP_VERBOSE: &str = "Example: Here is an example in the repl\n\
                                        \t>> (define m (map (quote ((ten . 10) (twenty . 20)))))\n\
                                        \t>> (get m)\n\
                                        \tLI>> ten: 10 \n\
                                        \ttwenty: 20 \n\
                                        \t>> (define m (set-map m (three . 3 )))\n\
                                        \tLI>> (get m)\n\
                                        \tLI>> ten: 10 \n\
                                        \tthree: 3\n\
                                        \ttwenty: 20";
}

pub fn map(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        0 => Ok(LValue::Map(Default::default())),
        1 => match args.get(0).unwrap() {
            LValue::List(list_sv) => {
                let mut facts: HashMap<LValue, LValue> = Default::default();
                for sv in list_sv {
                    match sv {
                        LValue::List(val_sv) => {
                            if val_sv.len() != 2 {
                                return Err(WrongNumberOfArgument(
                                    MAP,
                                    val_sv.into(),
                                    val_sv.len(),
                                    2..2,
                                ));
                            }

                            let key = val_sv[0].clone();
                            let value = val_sv[1].clone();
                            facts.insert(key, value);
                        }
                        lv => return Err(WrongType(MAP, lv.clone(), lv.into(), TypeLValue::List)),
                    }
                }
                Ok(LValue::Map(facts))
            }
            LValue::Nil => Ok(LValue::Map(Default::default())),
            lv => Err(WrongType(MAP, lv.clone(), lv.into(), TypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(MAP, args.into(), args.len(), 1..1)),
    }
}

pub fn get_map(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            GET_MAP,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }

    match &args[0] {
        LValue::Map(map) => {
            let key = &args[1];
            let value = map.get(key).unwrap_or(&LValue::Nil);
            Ok(value.clone())
        }
        lv => Err(WrongType(GET_MAP, lv.clone(), lv.into(), TypeLValue::Map)),
    }
}

pub fn set_map(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            SET_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 2 {
                    let key = val_sv.get(0).unwrap().clone();
                    let value = val_sv.get(1).unwrap().clone();
                    Ok(m.update(key, value).into())
                } else {
                    Err(WrongNumberOfArgument(
                        SET_MAP,
                        val_sv.into(),
                        val_sv.len(),
                        2..2,
                    ))
                }
            }
            lv => Err(WrongType(SET_MAP, lv.clone(), lv.into(), TypeLValue::List)),
        },
        lv => Err(WrongType(SET_MAP, lv.clone(), lv.into(), TypeLValue::Map)),
    }
}

pub fn remove_key_value_map(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            REMOVE_KEY_VALUE_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 2 {
                    let key = val_sv.get(0).unwrap().clone();
                    let value = val_sv.get(1).unwrap().clone();
                    match m.get(&key) {
                        None => {
                            return Err(SpecialError(
                                REMOVE_KEY_VALUE_MAP,
                                format!("map does not contain key {}", key),
                            ))
                        }
                        Some(v) => {
                            if *v == value {
                                let mut m = m.clone();
                                m.remove(&key);
                                Ok(m.into())
                            } else {
                                Err(SpecialError(
                                    REMOVE_KEY_VALUE_MAP,
                                    format!("map does not have key value ({}:{})", key, value),
                                ))
                            }
                        }
                    }
                } else {
                    Err(WrongNumberOfArgument(
                        REMOVE_KEY_VALUE_MAP,
                        val_sv.into(),
                        val_sv.len(),
                        2..2,
                    ))
                }
            }
            lv => Err(WrongType(
                REMOVE_KEY_VALUE_MAP,
                lv.clone(),
                lv.into(),
                TypeLValue::List,
            )),
        },
        lv => Err(WrongType(
            REMOVE_KEY_VALUE_MAP,
            lv.clone(),
            lv.into(),
            TypeLValue::Map,
        )),
    }
}

pub fn remove_map(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            REMOVE_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => {
            let mut new_m = m.clone();
            new_m.remove(&args[1]);
            Ok(new_m.into())
        }
        lv => Err(WrongType(
            REMOVE_MAP,
            lv.clone(),
            lv.into(),
            TypeLValue::Map,
        )),
    }
}

/// Merges two hashmap tables
pub fn union_map(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            UNION_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let map1 = &args[0];
    let map2 = &args[1];

    if let LValue::Map(map1) = map1.clone() {
        if let LValue::Map(map2) = map2.clone() {
            Ok(map1.union(map2).into())
        } else {
            Err(WrongType(
                UNION_MAP,
                map2.clone(),
                map2.into(),
                TypeLValue::Map,
            ))
        }
    } else {
        Err(WrongType(
            UNION_MAP,
            map1.clone(),
            map1.into(),
            TypeLValue::Map,
        ))
    }
}
