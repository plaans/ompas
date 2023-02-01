pub mod r#struct;

use crate::conversion::flow::p_eval::r#struct::{PConfig, PLValue};
use anyhow::anyhow;
use async_recursion::async_recursion;
use sompas_core::{expand_quasi_quote, get_debug, parse_into_lvalue};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lprimitives::LPrimitives;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_type};
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;
use std::sync::Arc;

pub const P_EVAL: &str = "p-eval";
pub const P_EXPAND: &str = "p-expand";
pub const P_PARSE: &str = "p-parse";

#[async_recursion]
pub async fn p_eval(lv: &LValue, env: &mut LEnv, pc: &PConfig) -> lruntimeerror::Result<PLValue> {
    let mut lv = lv.clone();
    let mut temp_env: LEnv;
    let mut env = env;

    let str = format!("{}", lv);

    loop {
        if let LValue::Symbol(s) = &lv {
            let str = s.as_str();

            return if pc.avoid.contains(str) {
                Ok(PLValue::into_unpure(&lv))
            } else {
                let result = match pc.p_table.try_get_param(s.as_str()) {
                    None => {
                        let result = match env.get_symbol(s.as_str()) {
                            None => lv.clone(),
                            Some(lv) => lv,
                        };

                        PLValue::into_pure(&result)
                    }
                    Some(plv) => plv.clone(),
                };
                Ok(result)
            };
        } else if let LValue::List(list) = &lv {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            if let LValue::Primitive(co) = proc {
                match co {
                    LPrimitives::Define => {
                        return match &args[0] {
                            LValue::Symbol(s) => {
                                let result = p_eval(&args[1], env, pc).await?;
                                return if result.is_pure() {
                                    env.insert(s.to_string(), result.lvalue);
                                    if get_debug() {
                                        println!("{} => {}", str, LValue::Nil);
                                    }
                                    Ok(PLValue::into_pure(&LValue::Nil))
                                }
                                else {
                                    Ok(PLValue::into_unpure(&result.get_lvalue()))
                                }
                            }
                            lv => {
                                Err(wrong_type!(
                                    P_EVAL,
                                    lv,
                                    KindLValue::Symbol
                                ))
                            }
                        };

                    }
                    LPrimitives::DefLambda => {
                        //println!("it is a lambda");
                        let params = match &args[0] {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list.iter() {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv => {
                                            return Err(wrong_type!(
                                                P_EVAL,
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
                                    P_EVAL,
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
                    LPrimitives::If => {
                        let test = &args[0];
                        let conseq = &args[1];
                        let alt = &args[2];
                        let result = p_eval(test, env, pc).await?;
                        if result.is_pure() {
                            lv = match result.lvalue {
                                LValue::True => conseq.clone(),
                                LValue::Nil => alt.clone(),
                                lv => {
                                    return Err(wrong_type!(
                                        P_EVAL,
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
                    LPrimitives::Quote => {
                        if get_debug() {
                            println!("{} => {}", str, &args[0].clone());
                        }
                        return Ok(PLValue::into_pure(&args[0]));
                    }
                    LPrimitives::Begin | LPrimitives::Do => {
                        let _do = *co == LPrimitives::Do;
                        let mut elements: Vec<LValue> = vec![co.into()];
                        let mut all_pure = true;

                        for e in args {
                            let result: PLValue = p_eval(e, env, pc).await?;
                            all_pure &= result.is_pure();
                            if all_pure && _do && matches!(result.lvalue, LValue::Err(_)){
                                return Ok(result)
                            }
                            elements.push(result.lvalue)
                        }

                        return if all_pure {
                            Ok(PLValue::into_pure(elements.last().unwrap()))
                        }else {
                            Ok(PLValue::into_unpure(&elements.into()))
                        };
                    }
                    LPrimitives::QuasiQuote
                    | LPrimitives::UnQuote
                    | LPrimitives::DefMacro => return Err(lruntimeerror!(P_EVAL, "quasiquote, unquote and defmacro should not be prensent in exanded expressions")),
                    LPrimitives::Async | LPrimitives::Await  =>  {

                        let arg = &args[0];
                        let p_lvalue = p_eval(arg, env, pc).await?;

                        return Ok(PLValue::into_pure(&list![proc.clone(), p_lvalue.get_lvalue().clone()]))
                    }
                    LPrimitives::Eval => {
                        let arg = &args[0];
                        let result = p_eval(arg, env, pc).await?;
                        lv = if result.is_pure() {
                            let result = p_expand(result.get_lvalue(), true, env, pc).await?;
                            if  result.is_pure() {
                                result.get_lvalue().clone()
                            }else {
                                return Ok(PLValue::into_unpure(&lv))
                            }
                        }else {
                            return Ok(PLValue::into_unpure(&lv))
                        };
                    }
                    LPrimitives::Parse => {
                        let result = p_eval(&args[0], env, pc).await?;
                        return if result.is_pure() {
                            if let LValue::String(s) = result.lvalue {
                                p_parse(s.as_str(), env, pc).await
                            } else {
                                Err(wrong_type!(
                                    P_EVAL,
                                    &args[0],
                                    KindLValue::String
                                ))
                            }
                        }
                        else {
                            Ok(PLValue::into_unpure(&lv))
                        }

                    }
                    LPrimitives::Expand => {
                        let arg = &args[0];
                        let result: PLValue = p_eval(arg, env, pc).await?;
                        return if result.is_pure() {
                            p_expand(&result.lvalue, true, env, pc).await
                        }else {
                            Ok(PLValue::into_unpure(&lv))
                        }
                    }
                    LPrimitives::Err => {
                        let arg = &args[0];
                        let result: PLValue = p_eval(arg, env, pc).await?;
                        return if result.is_pure() {
                            p_expand(&result.lvalue, true, env, pc).await
                        }else {
                            Ok(PLValue::into_unpure(&lv))
                        }
                    }
                    co => panic!("p-eval of {} not supported yet", co),
                    /*LPrimitives::Interrupt => {}
                    LPrimitives::Interruptible => {}
                    LPrimitives::Uninterruptible => {}
                    LPrimitives::Enr => {}
                    LPrimitives::Race => {}*/
                }
            } else {
                let mut exps: Vec<PLValue> = vec![];

                let mut all_pure = true;

                let proc: PLValue = p_eval(&proc, env, pc).await?;
                let proc_is_pure: bool = proc.is_pure();
                exps.push(proc);

                for x in args {
                    let result = p_eval(x, env, pc).await?;
                    all_pure &= result.is_pure();
                    exps.push(result);
                }

                let exps: Vec<LValue> = exps.drain(..).map(|plv| plv.into()).collect();
                let proc = &exps[0];
                let args: &[LValue] = &exps[1..];
                if proc_is_pure {
                    match proc {
                        LValue::Lambda(l) => {
                            lv = l.get_body().clone();
                            temp_env = l.get_new_env(env.clone(), args)?;
                            env = &mut temp_env;
                        }
                        LValue::Fn(fun) => {
                            return if !(all_pure && env.get_pfc().is_pure(fun.get_label())) {
                                Ok(PLValue::into_unpure(&exps.into()))
                            } else {
                                let r_lvalue = fun.call(env, args)?;
                                if get_debug() {
                                    println!("{} => {}", str, r_lvalue);
                                }
                                return Ok(PLValue::into_pure(&r_lvalue));
                            }
                        }
                        LValue::AsyncFn(fun) => {
                            return if !(all_pure && env.get_pfc().is_pure(fun.get_label())) {
                                Ok(PLValue::into_unpure(&exps.into()))
                            } else {
                                let r_lvalue = fun.call(env, args).await?;
                                if get_debug() {
                                    println!("{} => {}", str, r_lvalue);
                                }
                                return Ok(PLValue::into_pure(&r_lvalue));
                            }
                        }
                        lv => {
                            return Err(wrong_type!(P_EVAL, lv, KindLValue::Fn));
                        }
                    };
                } else {
                    return Ok(PLValue::into_unpure(&exps.into()));
                }
            }
        } else {
            if get_debug() {
                println!("{} => {}", str, lv.clone());
            }
            return Ok(PLValue::into_pure(&lv));
        }
    }
}

#[async_recursion]
pub async fn p_expand(
    lv: &LValue,
    top_level: bool,
    p_env: &mut LEnv,
    p_table: &PConfig,
) -> lruntimeerror::Result<PLValue> {
    match lv {
        LValue::List(list) => {
            if let Ok(co) = LPrimitives::try_from(&list[0]) {
                match co {
                    LPrimitives::Define | LPrimitives::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                3..std::usize::MAX,
                            ));
                        }
                        let def = LPrimitives::try_from(&list[0])?;
                        let v = &list[1];
                        let body = &list[2..];
                        match v {
                            LValue::List(v_list) => {
                                if v_list.len() >= 2 {
                                    let f = &v_list[0];
                                    let args = &v_list[1..];
                                    let mut new_body = vec![LPrimitives::DefLambda.into()];
                                    new_body.append(&mut args.to_vec());
                                    new_body.append(&mut body.to_vec());
                                    return p_expand(
                                        &vec![def.into(), f.clone(), new_body.into()].into(),
                                        top_level,
                                        p_env,
                                        p_table,
                                    )
                                    .await;
                                }
                            }
                            LValue::Symbol(sym) => {
                                if list.len() != 3 {
                                    return Err(LRuntimeError::wrong_number_of_args(
                                        P_EXPAND,
                                        list,
                                        3..3,
                                    ));
                                }
                                let exp = p_expand(&list[2], top_level, p_env, p_table).await?;
                                if !exp.is_pure() {
                                    return Ok(PLValue::into_unpure(lv));
                                }
                                //println!("after expansion: {}", exp);
                                if def == LPrimitives::DefMacro {
                                    if !top_level {
                                        return Err(lruntimeerror!(
                                            P_EXPAND,
                                            format!("{}: defmacro only allowed at top level", lv)
                                        ));
                                    }
                                    let proc =
                                        p_eval(&exp.into(), &mut p_env.clone(), p_table).await?;
                                    //println!("new macro: {}", proc);
                                    if !matches!(proc.get_lvalue(), LValue::Lambda(_)) {
                                        return Err(lruntimeerror!(
                                            P_EXPAND,
                                            format!("{}: macro must be a procedure", proc)
                                        ));
                                    } else {
                                        p_env.add_macro(
                                            sym.to_string(),
                                            proc.get_lvalue().try_into()?,
                                        );
                                    }
                                    //println!("macro added");
                                    //Add to macro_table
                                    return Ok(PLValue::into_pure(&LValue::Nil));
                                }
                                //We add to the list the expanded body
                                return Ok(PLValue::into_pure(
                                    &vec![
                                        LPrimitives::Define.into(),
                                        v.clone(),
                                        exp.get_lvalue().clone(),
                                    ]
                                    .into(),
                                ));
                            }
                            _ => return Err(wrong_type!(P_EXPAND, lv, KindLValue::Symbol)),
                        }
                    }
                    LPrimitives::DefLambda => {
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
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
                                            P_EXPAND,
                                            format!("illegal lambda argument list: {}", lv)
                                        ));
                                    }
                                }
                            }
                            LValue::Symbol(_) | LValue::Nil => {}
                            lv => {
                                return Err(LRuntimeError::not_in_list_of_expected_types(
                                    P_EXPAND,
                                    lv,
                                    vec![KindLValue::List, KindLValue::Symbol],
                                ))
                            }
                        }
                        let exp = if body.len() == 1 {
                            body[0].clone()
                        } else {
                            let mut vec = vec![LPrimitives::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(Arc::new(vec))
                        };
                        let result = p_expand(&exp, top_level, p_env, p_table).await?;
                        if result.is_pure() {
                            return Ok(PLValue::into_unpure(lv));
                        }
                        return Ok(PLValue::into_pure(
                            &vec![LPrimitives::DefLambda.into(), vars.clone(), result.lvalue]
                                .into(),
                        ));
                    }
                    LPrimitives::If => {
                        let mut list = list.deref().clone();
                        if list.len() == 3 {
                            list.push(LValue::Nil);
                        }
                        if list.len() != 4 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                4..4,
                            ));
                        }
                        //return map(expand, x)
                        let mut expanded_list = vec![LPrimitives::If.into()];
                        for x in &list[1..] {
                            let result = p_expand(x, false, p_env, p_table).await?;
                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(x));
                            }
                            expanded_list.push(result.lvalue)
                        }
                        return Ok(PLValue::into_pure(&expanded_list.into()));
                    }
                    LPrimitives::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ));
                        }
                        return Ok(PLValue::into_pure(
                            &vec![LPrimitives::Quote.into(), list[1].clone()].into(),
                        ));
                    }
                    LPrimitives::Begin | LPrimitives::Do => {
                        return if list.len() == 1 {
                            Ok(PLValue::into_pure(&LValue::Nil))
                        } else {
                            let mut expanded_list = vec![co.into()];
                            for e in &list[1..] {
                                let result = p_expand(e, top_level, p_env, p_table).await?;
                                if !result.is_pure() {
                                    return Ok(PLValue::into_unpure(lv));
                                }
                                expanded_list.push(result.lvalue)
                            }
                            Ok(PLValue::into_pure(&expanded_list.into()))
                        }
                    }
                    LPrimitives::QuasiQuote => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            /*let expanded = expand_quasi_quote(&list[1], env)?;
                            //println!("{}", expanded);
                            //to expand quasiquote recursively
                            expand(&expanded, top_level, env, ctxs);*/
                            p_expand(
                                &expand_quasi_quote(&list[1], p_env)?,
                                top_level,
                                p_env,
                                p_table,
                            )
                            .await
                            //Ok(expanded)
                        };
                    }
                    LPrimitives::UnQuote => {
                        return Err(anyhow!(
                            "unquote must be inside a quasiquote expression".to_string(),
                        )
                        .into())
                    }
                    LPrimitives::Async => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitives::Async.into()];
                            let result = p_expand(&list[1], top_level, p_env, p_table).await?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(lv));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_unpure(&expanded.into()))
                        }
                    }
                    LPrimitives::Await => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitives::Await.into()];
                            let result = p_expand(&list[1], top_level, p_env, p_table).await?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(lv));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitives::Eval => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ))
                        } else {
                            let mut expanded = vec![LPrimitives::Eval.into()];
                            let result = p_expand(&list[1], top_level, p_env, p_table).await?;
                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(lv));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitives::Parse => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(P_EXPAND, list, 2..2))
                        } else {
                            let mut expanded = vec![LPrimitives::Parse.into()];
                            let result = p_expand(&list[1], top_level, p_env, p_table).await?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(lv));
                            }
                            expanded.push(result.lvalue);
                            Ok(PLValue::into_pure(&expanded.into()))
                        }
                    }
                    LPrimitives::Expand => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(P_EXPAND, list, 2..2))
                        } else {
                            let mut expanded = vec![LPrimitives::Expand.into()];
                            let result = p_expand(&list[1], top_level, p_env, p_table).await?;

                            if !result.is_pure() {
                                return Ok(PLValue::into_unpure(lv));
                            }

                            expanded.push(result.lvalue);
                            Ok(PLValue::into_unpure(&expanded.into()))
                        }
                    }
                    co => panic!("{} not yet supported", co),
                }
            } else if let LValue::Symbol(sym) = &list[0] {
                match p_env.get_macro(sym) {
                    None => {}
                    Some(m) => {
                        let mut new_env = m.get_new_env(p_env.clone(), &list[1..])?;
                        let result = p_eval(m.get_body(), &mut new_env, p_table).await?;
                        if !result.is_pure() {
                            return Ok(PLValue::into_unpure(lv));
                        }

                        let expanded = p_expand(&result.lvalue, top_level, p_env, p_table).await?;
                        if get_debug() {
                            println!("In expand: macro expanded: {:?}", expanded);
                        }
                        return Ok(expanded);
                    }
                }
            }

            let mut expanded_list: Vec<LValue> = vec![];
            for e in list.iter() {
                let result = p_expand(e, false, p_env, p_table).await?;
                if result.is_pure() {
                    expanded_list.push(result.lvalue);
                } else {
                    return Ok(PLValue::into_unpure(lv));
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

pub async fn p_parse(
    str: &str,
    env: &mut LEnv,
    p_table: &PConfig,
) -> lruntimeerror::Result<PLValue> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => p_expand(&parse_into_lvalue(&se), true, env, p_table).await,
        Err(e) => Err(lruntimeerror!(P_PARSE, format!("Error in command: {}", e))),
    }
}
