use im::HashMap;
use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lerror::LRuntimeError::*;
use sompas_structs::lfn;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;

lfn! {pub map(args, _){
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
                        lv => return Err(WrongType(MAP, lv.clone(), lv.into(), KindLValue::List)),
                    }
                }
                Ok(LValue::Map(facts))
            }
            LValue::Nil => Ok(LValue::Map(Default::default())),
            lv => Err(WrongType(MAP, lv.clone(), lv.into(), KindLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(MAP, args.into(), args.len(), 1..1)),
    }
}
    }

lfn! {pub get_map(args, _){
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
        lv => Err(WrongType(GET_MAP, lv.clone(), lv.into(), KindLValue::Map)),
    }
}
    }

lfn! {pub set_map(args, _){
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
            lv => Err(WrongType(SET_MAP, lv.clone(), lv.into(), KindLValue::List)),
        },
        lv => Err(WrongType(SET_MAP, lv.clone(), lv.into(), KindLValue::Map)),
    }
}
    }

lfn! {pub remove_key_value_map(args, _){
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
                            return Err(Anyhow(
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
                                Err(Anyhow(
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
                KindLValue::List,
            )),
        },
        lv => Err(WrongType(
            REMOVE_KEY_VALUE_MAP,
            lv.clone(),
            lv.into(),
            KindLValue::Map,
        )),
    }
}
    }

lfn! {pub remove_map(args, _){
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
            KindLValue::Map,
        )),
    }
}
    }

/// Merges two hashmap tables
lfn! {pub union_map(args, _){
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
                KindLValue::Map,
            ))
        }
    } else {
        Err(WrongType(
            UNION_MAP,
            map1.clone(),
            map1.into(),
            KindLValue::Map,
        ))
    }
}
    }
