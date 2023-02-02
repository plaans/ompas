use crate::{expand_quasi_quote, get_debug, parse_into_lvalue};
use anyhow::anyhow;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub const EVAL_STATIC: &str = "eval-static";
pub const EXPAND_STATIC: &str = "expand-static";
pub const PARSE_STATIC: &str = "parse_static";

#[derive(Clone, Debug)]
pub struct PLValue {
    lvalue: LValue,
    pure: bool,
}

pub struct PEnv {}

impl PLValue {
    pub fn is_pure(&self) -> bool {
        self.pure
    }
}

impl PLValue {
    pub fn get_lvalue(&self) -> &LValue {
        &self.lvalue
    }
}

impl PLValue {
    pub fn into_pure(lv: &LValue) -> PLValue {
        PLValue {
            lvalue: lv.clone(),
            pure: true,
        }
    }

    pub fn into_unpure(lv: &LValue) -> PLValue {
        PLValue {
            lvalue: lv.clone(),
            pure: false,
        }
    }
}

impl Default for PLValue {
    fn default() -> Self {
        Self {
            lvalue: LValue::Nil,
            pure: true,
        }
    }
}

impl Display for PLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lvalue)
    }
}

impl From<PLValue> for LValue {
    fn from(pl: PLValue) -> Self {
        pl.lvalue
    }
}

impl From<&PLValue> for LValue {
    fn from(pl: &PLValue) -> Self {
        pl.clone().into()
    }
}

pub fn p_expand(x: &LValue, top_level: bool, env: &mut LEnv) -> lruntimeerror::Result<PLValue> {
    match x {
        LValue::List(list) => {
            if let Ok(co) = LPrimitive::try_from(&list[0]) {
                match co {
                    LPrimitive::Define | LPrimitive::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                3..std::usize::MAX,
                            ));
                        }
                        let def = LPrimitive::try_from(&list[0])?;
                        let v = &list[1];
                        let body = &list[2..];
                        match v {
                            LValue::List(v_list) => {
                                if v_list.len() >= 2 {
                                    let f = &v_list[0];
                                    let args = &v_list[1..];
                                    let mut new_body = vec![LPrimitive::DefLambda.into()];
                                    new_body.append(&mut args.to_vec());
                                    new_body.append(&mut body.to_vec());
                                    return p_expand(
                                        &vec![def.into(), f.clone(), new_body.into()].into(),
                                        top_level,
                                        env,
                                    );
                                }
                            }
                            LValue::Symbol(sym) => {
                                if list.len() != 3 {
                                    return Err(LRuntimeError::wrong_number_of_args(
                                        EXPAND_STATIC,
                                        list,
                                        3..3,
                                    ));
                                }
                                let exp = p_expand(&list[2], top_level, env)?;
                                if !exp.is_pure() {
                                    return Ok(PLValue::into_unpure(x));
                                }
                                //println!("after expansion: {}", exp);
                                if def == LPrimitive::DefMacro {
                                    if !top_level {
                                        return Err(lruntimeerror!(
                                            EXPAND_STATIC,
                                            format!("{}: defmacro only allowed at top level", x)
                                        ));
                                    }
                                    let proc = p_eval(&exp.into(), &mut env.clone())?;
                                    //println!("new macro: {}", proc);
                                    if !matches!(proc.lvalue, LValue::Lambda(_)) {
                                        return Err(lruntimeerror!(
                                            EXPAND_STATIC,
                                            format!("{}: macro must be a procedure", proc)
                                        ));
                                    } else {
                                        env.add_macro(sym.to_string(), proc.lvalue.try_into()?);
                                    }
                                    //println!("macro added");
                                    //Add to macro_table
                                    return Ok(PLValue::into_pure(&LValue::Nil));
                                }
                                //We add to the list the expanded body
                                return Ok(PLValue::into_pure(
                                    &vec![LPrimitive::Define.into(), v.clone(), exp.lvalue].into(),
                                ));
                            }
                            _ => return Err(wrong_type!(EXPAND_STATIC, x, KindLValue::Symbol)),
                        }
                    }
                    LPrimitive::DefLambda => {
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list,
                                3..usize::MAX,
                            ));
                        }
                        let vars = &list[1];
                        let body = &list[2..];
                        //Verification of the types of the arguments
                        match vars {
                            LValue::List(vars_list) => {
                                for v in vars_list.iter() {
                                    if !matches!(v, LValue::Symbol(_)) {
                                        return Err(lruntimeerror!(
                                            EXPAND_STATIC,
                                            format!("illegal lambda argument list: {}", x)
                                        ));
                                    }
                                }
                            }
                            LValue::Symbol(_) | LValue::Nil => {}
                            lv => {
                                return Err(LRuntimeError::not_in_list_of_expected_types(
                                    EXPAND_STATIC,
                                    lv,
                                    vec![KindLValue::List, KindLValue::Symbol],
                                ))
                            }
                        }
                        let exp = if body.len() == 1 {
                            body[0].clone()
                        } else {
                            let mut vec = vec![LPrimitive::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(Arc::new(vec))
                        };
                        let result = p_expand(&exp, top_level, env)?;
                        if result.is_pure() {
                            return Ok(PLValue::into_unpure(x));
                        }
                        return Ok(PLValue::into_pure(
                            &vec![LPrimitive::DefLambda.into(), vars.clone(), result.lvalue].into(),
                        ));
                    }
                    LPrimitive::If => {
                        let mut list = list.deref().clone();
                        if list.len() == 3 {
                            list.push(LValue::Nil);
                        }
                        if list.len() != 4 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                4..4,
                            ));
                        }
                        //return map(expand, x)
                        let mut expanded_list = vec![LPrimitive::If.into()];
                        for x in &list[1..] {
                            let result = p_expand(x, false, env)?;
                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded_list.push(result.lvalue)
                        }
                        return Ok(PLValue::into_pure(&expanded_list.into()));
                    }
                    LPrimitive::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                2..2,
                            ));
                        }
                        return Ok(PLValue::into_pure(
                            &vec![LPrimitive::Quote.into(), list[1].clone()].into(),
                        ));
                    }
                    LPrimitive::Begin | LPrimitive::Do => {
                        return if list.len() == 1 {
                            Ok(PLValue::into_pure(&LValue::Nil))
                        } else {
                            let mut expanded_list = vec![co.into()];
                            for e in &list[1..] {
                                let result = p_expand(e, top_level, env)?;
                                if !result.is_pure() {
                                    return Ok(PLValue::into_unpure(x));
                                }
                                expanded_list.push(result.lvalue)
                            }
                            Ok(PLValue::into_pure(&expanded_list.into()))
                        }
                    }
                    LPrimitive::QuasiQuote => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            /*let expanded = expand_quasi_quote(&list[1], env)?;
                            //println!("{}", expanded);
                            //to expand quasiquote recursively
                            expand(&expanded, top_level, env, ctxs);*/
                            p_expand(&expand_quasi_quote(&list[1], env)?, top_level, env)
                            //Ok(expanded)
                        };
                    }
                    LPrimitive::UnQuote => {
                        return Err(anyhow!(
                            "unquote must be inside a quasiquote expression".to_string(),
                        )
                        .into())
                    }
                    LPrimitive::Async => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitive::Async.into()];
                            let result = p_expand(&list[1], top_level, env)?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_unpure(&expanded.into()))
                        }
                    }
                    LPrimitive::Await => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitive::Await.into()];
                            let result = p_expand(&list[1], top_level, env)?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitive::Eval => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitive::Eval.into()];
                            let result = p_expand(&list[1], top_level, env)?;
                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitive::Parse => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list,
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitive::Parse.into()];
                            let result = p_expand(&list[1], top_level, env)?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitive::Expand => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                EXPAND_STATIC,
                                list,
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitive::Expand.into()];
                            let result = p_expand(&list[1], top_level, env)?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }

                            expanded.push(result.lvalue);
                            Ok(PLValue::into_unpure(&expanded.into()))
                        }
                    }
                    co => panic!("{} not yet supported", co),
                }
            } else if let LValue::Symbol(sym) = &list[0] {
                match env.get_macro(sym) {
                    None => {}
                    Some(m) => {
                        let mut new_env = m.get_new_env(env.clone(), &list[1..])?;
                        let result = p_eval(m.get_body(), &mut new_env)?;
                        if !result.is_pure() {
                            return Ok(PLValue::into_unpure(x));
                        }

                        let expanded = p_expand(&result.lvalue, top_level, env)?;
                        if get_debug() {
                            println!("In expand: macro expanded: {:?}", expanded);
                        }
                        return Ok(expanded);
                    }
                }
            }

            let mut expanded_list: Vec<LValue> = vec![];
            for e in list.iter() {
                let result = p_expand(e, false, env)?;
                if result.is_pure() {
                    expanded_list.push(result.lvalue);
                } else {
                    return Ok(PLValue::into_unpure(x));
                }
            }

            /*let expanded_list: Vec<LValue> = list
            .iter()
            .map(|x| expand(x, false, env, ctxs))
            .collect::<Result<_, _>>()?;*/
            Ok(PLValue::into_pure(&expanded_list.into()))
        }
        lv => Ok(PLValue::into_pure(lv)),
    }
}

pub fn p_parse(str: &str, env: &mut LEnv) -> lruntimeerror::Result<PLValue> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => p_expand(&parse_into_lvalue(&se), true, env),
        Err(e) => Err(lruntimeerror!(
            PARSE_STATIC,
            format!("Error in command: {}", e)
        )),
    }
}

pub fn p_eval(lv: &LValue, env: &mut LEnv) -> lruntimeerror::Result<PLValue> {
    let mut lv = lv.clone();
    let mut temp_env: LEnv;
    let mut env = env;

    let str = format!("{}", lv);

    loop {
        if let LValue::Symbol(s) = &lv {
            let result = match env.get_symbol(s.as_str()) {
                None => lv.clone(),
                Some(lv) => lv,
            };
            match result {
                LValue::Fn(_) => {
                    if !env.get_pfc().is_pure(s) {
                        return Ok(PLValue::into_unpure(&lv));
                    }
                }
                LValue::AsyncFn(_) => return Ok(PLValue::into_unpure(&lv)),
                _ => {}
            }
            if get_debug() {
                println!("{} => {}", str, result)
            }

            return Ok(PLValue::into_pure(&result));
        } else if let LValue::List(list) = &lv {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            if let LValue::Primitive(co) = proc {
                match co {
                    LPrimitive::Define => {
                        return match &args[0] {
                            LValue::Symbol(s) => {
                                let result = p_eval(&args[1], env)?;
                                return if result.pure {
                                    env.insert(s.to_string(), result.lvalue);
                                    if get_debug() {
                                        println!("{} => {}", str, LValue::Nil);
                                    }
                                    Ok(PLValue::into_pure(&LValue::Nil))
                                }
                                else {
                                    Ok(PLValue::into_unpure(&lv))
                                }
                            }
                            lv => {
                                Err(wrong_type!(
                                    EVAL_STATIC,
                                    lv,
                                    KindLValue::Symbol
                                ))
                            }
                        };

                    }
                    LPrimitive::DefLambda => {
                        //println!("it is a lambda");
                        let params = match &args[0] {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list.iter() {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv => {
                                            return Err(wrong_type!(
                                                EVAL_STATIC,
                                                lv,
                                                KindLValue::Symbol
                                            ))
                                        }
                                    }
                                }
                                vec_sym.into()
                            }
                            LValue::Symbol(s) => s.clone().into(),
                            LValue::Nil => LambdaArgs::Nil,
                            lv => {
                                return Err(LRuntimeError::not_in_list_of_expected_types(
                                    EVAL_STATIC,
                                    lv,
                                    vec![KindLValue::List, KindLValue::Symbol],
                                ))
                            }
                        };
                        let body = &args[1];
                        let r_lvalue =
                            LValue::Lambda(LLambda::new(params, body.clone(), env.get_symbols()));
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        return Ok(PLValue::into_pure(&r_lvalue));
                    }
                    LPrimitive::If => {
                        let test = &args[0];
                        let conseq = &args[1];
                        let alt = &args[2];
                        let result = p_eval(test, env)?;
                        if result.pure {
                            lv = match result.lvalue {
                                LValue::True => conseq.clone(),
                                LValue::Nil => alt.clone(),
                                lv => {
                                    return Err(wrong_type!(
                                        EVAL_STATIC,
                                        &lv,
                                        KindLValue::Bool
                                    ))
                                }
                            };
                        }
                        else {
                            return Ok(PLValue::into_unpure(&lv))
                        }

                    }
                    LPrimitive::Quote => {
                        if get_debug() {
                            println!("{} => {}", str, &args[0].clone());
                        }
                        return Ok(PLValue::into_pure(&args[0]));
                    }
                    LPrimitive::Begin | LPrimitive::Do => {
                        let _do = *co == LPrimitive::Do;
                        let mut elements = vec![];
                        let mut all_pure = true;

                        for e in args {
                            if all_pure {
                                let result = p_eval(e, env)?;
                                all_pure = result.pure;
                                if result.pure && _do && matches!(result.lvalue, LValue::Err(_)){
                                    return Ok(result)
                                }
                                elements.push(result.lvalue)
                            }else {
                                elements.push(e.clone())
                            }
                        }
                        return if all_pure {
                            Ok(PLValue::into_pure(elements.last().unwrap()))
                        }else {
                            Ok(PLValue::into_unpure(&elements.into()))
                        };
                    }
                    LPrimitive::QuasiQuote
                    | LPrimitive::UnQuote
                    | LPrimitive::DefMacro => return Err(lruntimeerror!(EVAL_STATIC, "quasiquote, unquote and defmacro should not be prensent in exanded expressions")),
                    LPrimitive::Async | LPrimitive::Await => {
                        return Ok(PLValue::into_unpure(&lv));
                    }
                    LPrimitive::Eval => {
                        let arg = &args[0];
                        let result = p_eval(arg, env)?;
                        lv = if result.is_pure() {
                            let result = p_expand(&result.lvalue, true, env)?;
                            if  result.is_pure() {
                                result.lvalue
                            }else {
                                return Ok(PLValue::into_unpure(&lv))
                            }
                        }else {
                            return Ok(PLValue::into_unpure(&lv))
                        };
                    }
                    LPrimitive::Parse => {
                        let result = p_eval(&args[0], env)?;
                        return if result.is_pure() {
                            if let LValue::String(s) = result.lvalue {
                                p_parse(s.as_str(), env)
                            } else {
                                Err(wrong_type!(
                                    EVAL_STATIC,
                                    &args[0],
                                    KindLValue::String
                                ))
                            }
                        }
                        else {
                            Ok(PLValue::into_unpure(&lv))
                        }

                    }
                    LPrimitive::Expand => {
                        let arg = &args[0];
                        let result = p_eval(arg, env)?;
                        return if result.is_pure() {
                            p_expand(&result.lvalue, true, env)
                        }else {
                            Ok(PLValue::into_unpure(&lv))
                        }
                    }
                    co => panic!("{} not yet supported", co)
                }
            } else {
                let mut exps: Vec<PLValue> = vec![];

                let mut all_pure = true;

                for x in list {
                    let result = p_eval(x, env)?;
                    all_pure &= result.is_pure();
                    exps.push(result);
                }

                if !all_pure {
                    return Ok(PLValue::into_unpure(&exps.into()));
                }

                /*let exps = list
                .iter()
                .map(|x| eval(x, &mut env, ctxs).await)
                .collect::<Result<Vec<LValue>, _>>()?;*/
                let proc = &exps[0];
                let args: Vec<LValue> = exps[1..].iter().map(|plv| plv.into()).collect();
                match &proc.lvalue {
                    LValue::Lambda(l) => {
                        lv = l.get_body().clone();
                        temp_env = l.get_new_env(env.clone(), args.as_slice())?;
                        env = &mut temp_env;
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = fun.call(env, args.as_slice())?;
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        return Ok(PLValue::into_pure(&r_lvalue));
                    }
                    LValue::AsyncFn(_) => {
                        unreachable!("should have been detected unpure before")
                    }
                    lv => {
                        return Err(wrong_type!(EVAL_STATIC, lv, KindLValue::Fn));
                    }
                };
            }
        } else {
            if get_debug() {
                println!("{} => {}", str, lv.clone());
            }
            return Ok(PLValue::into_pure(&lv));
        }
    }
}
