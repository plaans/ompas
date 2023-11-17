pub mod r#struct;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::{
    PBeginFrame, PCoreOperatorFrame, PDefineFrame, PDoFrame, PEvalStack, PIfFrame, PLDebug, PLEnv,
    PLValue, PProcedureFrame, PResults, PScopeCollection, PStackFrame, PUnstack,
};
use crate::planning::conversion::flow_graph::algo::pre_processing::transform_lambda_expression;
use anyhow::anyhow;
use async_recursion::async_recursion;
use sompas_core::{expand_quasi_quote, parse_into_lvalue};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, string, wrong_type};
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;
use std::sync::Arc;

pub const P_EVAL: &str = "p-eval";
pub const P_EXPAND: &str = "p-expand";
pub const P_PARSE: &str = "p-parse";

/// Pre evaluate a LValue
/// Main function of the Scheme Interpreter
#[async_recursion]
pub async fn p_eval(lv: &LValue, root_env: &mut PLEnv) -> LResult {
    let log = root_env.env.log.clone();
    let mut debug: PLDebug = Default::default();
    debug.log = log.clone();
    debug.push(lv);

    let mut queue: PEvalStack = Default::default();
    queue.push(lv.clone());

    let mut scopes: PScopeCollection = PScopeCollection::new(root_env);
    let mut results: PResults = Default::default();

    let mut expression_error = LValue::Nil;

    let result: LResult = 'main: loop {
        let current = match queue.pop() {
            Some(lv) => lv,
            None => break Ok(results.pop().unwrap().into()),
        };

        match current {
            PStackFrame::NonEvaluated(ref lv) => {
                debug.push(lv.to_string());
                match &lv {
                    LValue::Symbol(s) => {
                        let plenv = scopes.get_last();
                        let str = s.as_str();
                        let result = if plenv.get_p_config().avoid.contains(str) {
                            PLValue::unpure(lv.into())
                        } else {
                            match plenv.get_unpure(str) {
                                None => match plenv.get_env().get_symbol(str) {
                                    None => {
                                        match plenv.get_p_config().p_table.try_get_param(s.as_str())
                                        {
                                            None => PLValue::pure(lv.clone()),
                                            Some(plv) => plv.clone(),
                                        }
                                    }
                                    Some(lv) => PLValue::pure(lv),
                                },
                                Some(plv) => plv.clone(),
                            }
                        };
                        results.push(result);
                        debug.log_last_result(&results);
                    }
                    LValue::List(list) => {
                        let list = list.as_slice();
                        let proc = &list[0];
                        let args = &list[1..];
                        if let LValue::Primitive(co) = proc {
                            match co {
                                LPrimitive::Define => {
                                    match &args[0] {
                                        LValue::Symbol(s) => {
                                            scopes.new_scope();
                                            queue.push(PDefineFrame { symbol: s.clone() });
                                            queue.push(args[1].clone());
                                        }
                                        lv => {
                                            let err = Err(wrong_type!(
                                                "eval",
                                                &lv.clone(),
                                                KindLValue::Symbol
                                            ));
                                            expression_error = current.unstack(&mut results).into();
                                            break 'main err;
                                        }
                                    };
                                }
                                LPrimitive::DefLambda => {
                                    let params = match &args[0] {
                                        LValue::List(list) => {
                                            let mut vec_sym = Vec::new();
                                            for val in list.iter() {
                                                match val {
                                                    LValue::Symbol(s) => vec_sym.push(s.clone()),
                                                    lv => {
                                                        return Err(wrong_type!(
                                                            "eval",
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
                                            let err = LRuntimeError::not_in_list_of_expected_types(
                                                P_EVAL,
                                                &lv.clone(),
                                                vec![KindLValue::List, KindLValue::Symbol],
                                            );
                                            expression_error = current.unstack(&mut results).into();

                                            break Err(err);
                                        }
                                    };
                                    let body = &args[1];
                                    results.push(
                                        LValue::Lambda(LLambda::new(
                                            params,
                                            body.clone(),
                                            scopes.get_last().env.get_symbols(),
                                        ))
                                        .into(),
                                    );
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::If => {
                                    scopes.new_scope();
                                    let stack = PIfFrame {
                                        conseq: args[1].clone(),
                                        alt: args[2].clone(),
                                    };

                                    queue.push(stack);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Quote => {
                                    results.push(args[0].clone().into());
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::Err => {
                                    results.push(PLValue::unpure(LValue::Err(
                                        args[0].clone().into_ref(),
                                    )));
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::QuasiQuote => {
                                    panic!("quasiquote not allowed")
                                }
                                LPrimitive::UnQuote => {
                                    panic!("unquote not allowed")
                                }
                                LPrimitive::DefMacro => {
                                    panic!("defmacro not allowed")
                                }
                                LPrimitive::Begin => {
                                    scopes.new_scope();
                                    queue.push(PBeginFrame { n: args.len() });
                                    queue
                                        .push_list(args.iter().map(|a| a.clone().into()).collect());
                                }
                                LPrimitive::Do => {
                                    scopes.new_scope();

                                    queue.push(PDoFrame {
                                        results: vec![],
                                        rest: args[1..].to_vec(),
                                        pure: true,
                                    });
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Eval => {
                                    queue.push(PCoreOperatorFrame::Eval);
                                    queue.push(PCoreOperatorFrame::Expand);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Enr => {
                                    queue.push(PCoreOperatorFrame::Enr);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Expand => {
                                    queue.push(PCoreOperatorFrame::Expand);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Parse => {
                                    queue.push(PCoreOperatorFrame::Parse);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Interruptible => {
                                    queue.push(PCoreOperatorFrame::Interruptible);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Uninterruptible => {
                                    queue.push(PCoreOperatorFrame::Uninterruptible);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Async => {
                                    queue.push(PCoreOperatorFrame::Async);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Await => {
                                    queue.push(PCoreOperatorFrame::Await);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Race => {
                                    queue.push(PCoreOperatorFrame::Race);
                                    queue.push(args[0].clone());
                                }
                                LPrimitive::Interrupt => {
                                    queue.push(PCoreOperatorFrame::Interrupt);
                                    queue.push(args[0].clone());
                                }
                            }
                        } else if scopes
                            .get_last()
                            .env
                            .get_macro(proc.to_string().as_str())
                            .is_some()
                        {
                            queue.push(PCoreOperatorFrame::Eval);
                            queue.push(PCoreOperatorFrame::Expand);
                            results.push(PLValue::pure(lv.clone()));
                        } else {
                            scopes.new_scope();
                            queue.push(PProcedureFrame { n: list.len() });
                            queue.push_list(list.iter().map(|a| a.clone().into()).collect());
                        }
                    }
                    _ => {
                        results.push(lv.clone().into());
                        debug.log_last_result(&results);
                    }
                }
            }
            PStackFrame::Procedure(ref pro) => {
                let mut exps: Vec<PLValue> = results.pop_n(pro.n);
                let proc = &exps[0];
                let proc_is_pure: bool = proc.is_pure();
                let args = &exps[1..];
                let mut args_pure = true;
                let mut args_unpure = true;
                args.iter().for_each(|plv| {
                    args_pure &= plv.is_pure();
                    args_unpure &= !plv.is_pure();
                });
                if proc_is_pure {
                    match &proc.lvalue {
                        LValue::Lambda(l) => {
                            let p_env = scopes.get_last();

                            if args_pure {
                                queue.push(PCoreOperatorFrame::Lambda);
                                queue.push(l.get_body().clone());
                                let temp_env = match l.get_new_env(
                                    p_env.env.clone(),
                                    args.iter()
                                        .map(|plv| plv.lvalue.clone())
                                        .collect::<Vec<LValue>>()
                                        .as_slice(),
                                ) {
                                    Ok(e) => e,
                                    Err(e) => {
                                        expression_error = exps.into();
                                        break Err(e);
                                    }
                                };
                                let mut p_temp_env = p_env.clone();
                                p_temp_env.env = temp_env;
                                scopes.new_defined_scope(p_temp_env);
                            } else {
                                let mut new_exps = vec![];
                                for (i, exp) in exps.drain(..).enumerate() {
                                    if i != 0 {
                                        new_exps.push(exp.lvalue_as_quote())
                                    } else {
                                        new_exps.push(exp.lvalue)
                                    }
                                }
                                queue.push(PCoreOperatorFrame::Lambda);
                                queue.push(
                                    transform_lambda_expression(&new_exps.into(), p_env).await?,
                                );
                                scopes.new_scope();
                            }
                        }
                        LValue::Fn(fun) => {
                            scopes.revert_scope();
                            let env = &scopes.get_last().env;
                            let r_lvalue = if !(args_pure && env.get_pfc().is_pure(fun.get_label()))
                            {
                                let mut vec = vec![proc.lvalue.clone()];
                                vec.append(
                                    &mut exps[1..]
                                        .iter()
                                        .map(|plv| plv.clone().lvalue_as_quote())
                                        .collect(),
                                );

                                PLValue::unpure(vec.into())
                            } else {
                                match fun.call(
                                    env,
                                    args.iter()
                                        .map(|plv| plv.lvalue.clone())
                                        .collect::<Vec<LValue>>()
                                        .as_slice(),
                                ) {
                                    Ok(e) => e,
                                    Err(e) => {
                                        expression_error = exps.into();
                                        break Err(e);
                                    }
                                }
                                .into()
                            };

                            results.push(r_lvalue);
                            debug.log_last_result(&results);
                        }
                        LValue::AsyncFn(fun) => {
                            scopes.revert_scope();
                            let env = &scopes.get_last().env;
                            let r_lvalue = if !(args_pure && env.get_pfc().is_pure(fun.get_label()))
                            {
                                let mut vec = vec![proc.lvalue.clone()];
                                vec.append(
                                    &mut exps[1..]
                                        .iter()
                                        .map(|plv| plv.clone().lvalue_as_quote())
                                        .collect(),
                                );

                                PLValue::unpure(vec.into())
                            } else {
                                match fun
                                    .call(
                                        env,
                                        args.iter()
                                            .map(|plv| plv.lvalue.clone())
                                            .collect::<Vec<LValue>>()
                                            .as_slice(),
                                    )
                                    .await
                                {
                                    Ok(e) => e,
                                    Err(e) => {
                                        expression_error = exps.into();
                                        break Err(e);
                                    }
                                }
                                .into()
                            };

                            results.push(r_lvalue);
                            debug.log_last_result(&results);
                        }
                        lv => {
                            let e = wrong_type!("eval", lv, KindLValue::Fn);
                            expression_error = exps.into();
                            break Err(e);
                        }
                    }
                } else {
                    results.push(PLValue::unpure({
                        let mut vec = vec![proc.lvalue.clone()];
                        vec.append(
                            &mut exps[1..]
                                .iter()
                                .map(|plv| plv.clone().lvalue_as_quote())
                                .collect(),
                        );
                        vec.into()
                    }));
                }
            }
            PStackFrame::CoreOperator(cos) => match cos {
                PCoreOperatorFrame::Define(d) => {
                    scopes.revert_scope();
                    let p_env = scopes.get_last_mut();
                    let result = results.pop().unwrap();
                    let s = d.symbol.as_ref();
                    if result.is_pure() {
                        p_env.remove_unpure(s);
                        p_env.env.insert(s, result.lvalue);
                        results.push(LValue::Nil.into());
                    } else {
                        p_env.add_unpure(s.to_string());
                        results.push(PLValue::unpure(list![
                            LPrimitive::Define.into(),
                            s.into(),
                            result.lvalue_as_quote()
                        ]));
                    }
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::If(i) => {
                    let result = results.pop().unwrap();
                    if result.is_pure() {
                        match result.lvalue {
                            LValue::True => queue.push(i.conseq.clone()),
                            LValue::Nil => queue.push(i.alt.clone()),
                            lv => {
                                let e = wrong_type!("eval", &lv, KindLValue::Bool);
                                expression_error =
                                    list![LPrimitive::If.into(), lv, i.conseq.clone(), i.alt];
                                break Err(e.chain("if condition must return a boolean."));
                            }
                        };
                    } else {
                        scopes.new_scope();
                        let p_env = scopes.get_last_mut();
                        let conseq = p_eval(&i.conseq, p_env).await?;
                        let alt = p_eval(&i.alt, p_env).await?;
                        results.push(PLValue::unpure(list![
                            LPrimitive::If.into(),
                            result.lvalue_as_quote(),
                            conseq,
                            alt
                        ]));
                        scopes.revert_scope()
                    }

                    scopes.revert_scope()
                }
                PCoreOperatorFrame::Do(mut df) => {
                    let result = results.pop().unwrap();
                    df.pure &= result.is_pure();
                    if df.rest.is_empty() {
                        if df.pure {
                            results.push(result)
                        } else {
                            df.results.push(result);
                            let mut expr: Vec<LValue> = df
                                .results
                                .drain(..)
                                .map(|plv| plv.lvalue_as_quote())
                                .collect();
                            expr.insert(0, LPrimitive::Do.into());
                            results.push(PLValue::unpure(expr.into()))
                        }

                        scopes.revert_scope();
                    } else {
                        df.results.push(result.clone());
                        if df.pure && matches!(result.lvalue, LValue::Err(_)) {
                            results.push(result);
                            scopes.revert_scope();
                        } else {
                            let next = df.rest.remove(0);
                            queue.push(df);
                            queue.push(next);
                        }
                    }
                }
                PCoreOperatorFrame::Begin(b) => {
                    let mut r = results.pop_n(b.n);
                    let mut pure = true;

                    r.iter().for_each(|plv| pure &= plv.is_pure());

                    if pure {
                        results.push(r.pop().unwrap());
                    } else {
                        let mut expr: Vec<LValue> = vec![LPrimitive::Begin.into()];
                        expr.append(&mut r.drain(..).map(|plv| plv.lvalue_as_quote()).collect());
                        results.push(PLValue::unpure(expr.into()))
                    }

                    debug.log_last_result(&results);
                    scopes.revert_scope();
                }
                PCoreOperatorFrame::Lambda => {
                    scopes.revert_scope();
                    scopes.revert_scope();
                }
                PCoreOperatorFrame::Async => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Async.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::Await => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Await.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::Race => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Race.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::Interrupt => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Interrupt.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::Eval => {
                    //debug.print_last_result(&results);
                    let result = results.pop().unwrap();
                    queue.push(PCoreOperatorFrame::EvalEnd);
                    if result.is_pure() {
                        queue.push(result.lvalue);
                    } else {
                        results.push(PLValue::unpure(list![
                            LPrimitive::Eval.into(),
                            result.lvalue_as_quote()
                        ]))
                    }
                }
                PCoreOperatorFrame::Expand => {
                    let result = results.pop().unwrap();
                    if result.is_pure() {
                        results.push(p_expand(&result.lvalue, true, scopes.get_last_mut()).await?);
                        debug.push(list!(LPrimitive::Expand.into(), result.lvalue));
                        debug.log_last_result(&results);
                    } else {
                        results.push(PLValue::unpure(list!(
                            LPrimitive::Expand.into(),
                            result.lvalue_as_quote()
                        )));
                    }
                }
                PCoreOperatorFrame::Parse => {
                    let result = results.pop().unwrap();
                    if result.is_pure() {
                        if let LValue::String(s) = &result.lvalue {
                            results.push(p_parse(s.as_str(), scopes.get_last_mut()).await?);
                            debug.log_last_result(&results);
                        } else {
                            let e = wrong_type!("p_eval", &result.lvalue, KindLValue::String);
                            expression_error = list![LPrimitive::Parse.into(), result.lvalue];
                            break Err(e.chain("Parse argument must be a string."));
                        };
                    } else {
                        results.push(PLValue::unpure(list![
                            LPrimitive::Parse.into(),
                            result.lvalue_as_quote()
                        ]));
                    }
                }
                PCoreOperatorFrame::Enr => {
                    let expr: PLValue = results.pop().unwrap();
                    if expr.is_pure() {
                        let expr: LValue = match expr.lvalue {
                            LValue::List(list) => {
                                let mut new_expr = vec![list[0].clone()];
                                for e in &list.as_slice()[1..] {
                                    new_expr.push(list!(LPrimitive::Quote.into(), e.clone()))
                                }
                                new_expr.into()
                            }
                            expr => expr,
                        };
                        queue.push(PCoreOperatorFrame::EnrEnd);
                        queue.push(expr);
                    } else {
                        results.push(PLValue::unpure(list![
                            LPrimitive::Enr.into(),
                            expr.lvalue_as_quote()
                        ]));
                    }

                    debug.pop();
                }
                PCoreOperatorFrame::Interruptible => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Interruptible.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::Uninterruptible => {
                    let last_result = results.pop().unwrap();
                    results.push(PLValue::unpure(list![
                        LPrimitive::Uninterruptible.into(),
                        last_result.lvalue_as_quote()
                    ]));
                    debug.log_last_result(&results);
                }
                PCoreOperatorFrame::EvalEnd | PCoreOperatorFrame::EnrEnd => {
                    debug.log_last_result(&results);
                }
            },
        }
    };

    match result {
        Ok(_) => result,
        Err(e) => unstack(expression_error, e, results, queue),
    }
}

fn unstack(
    current: LValue,
    e: LRuntimeError,
    mut results: PResults,
    mut queue: PEvalStack,
) -> LResult {
    results.push(string!(format!("[{} => {}]", current, e.get_message())).into());

    //loop to unstack and print where the error occured

    while let Some(s) = queue.pop() {
        let result = s.unstack(&mut results);
        results.push(result);
    }
    //Err(e.chain(format!("Scheme :\n{}", results.pop().unwrap().lvalue).as_str()))
    Err(e.chain(format!("Scheme :\n{}", results.pop().unwrap().lvalue.format(0)).as_str()))
}

#[async_recursion]
pub async fn p_expand(
    lv: &LValue,
    top_level: bool,
    p_env: &mut PLEnv,
) -> lruntimeerror::Result<PLValue> {
    match lv {
        LValue::List(list) => {
            if let Ok(co) = LPrimitive::try_from(&list[0]) {
                match co {
                    LPrimitive::Define | LPrimitive::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
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
                                        p_env,
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
                                let exp = p_expand(&list[2], top_level, p_env).await?;
                                if !exp.is_pure() {
                                    return Ok(PLValue::unpure(lv.clone()));
                                }
                                //println!("after expansion: {}", exp);
                                if def == LPrimitive::DefMacro {
                                    if !top_level {
                                        return Err(lruntimeerror!(
                                            P_EXPAND,
                                            format!("{}: defmacro only allowed at top level", lv)
                                        ));
                                    }
                                    let proc = p_eval(&exp.into(), &mut p_env.clone()).await?;
                                    //println!("new macro: {}", proc);
                                    if !matches!(proc, LValue::Lambda(_)) {
                                        return Err(lruntimeerror!(
                                            P_EXPAND,
                                            format!("{}: macro must be a procedure", proc)
                                        ));
                                    } else {
                                        p_env.env.add_macro(sym.to_string(), proc.try_into()?);
                                    }
                                    //println!("macro added");
                                    //Add to macro_table
                                    return Ok(PLValue::pure(LValue::Nil));
                                }
                                //We add to the list the expanded body
                                return Ok(PLValue::pure(
                                    vec![LPrimitive::Define.into(), v.clone(), exp.get_lvalue()]
                                        .into(),
                                ));
                            }
                            _ => return Err(wrong_type!(P_EXPAND, lv, KindLValue::Symbol)),
                        }
                    }
                    LPrimitive::DefLambda => {
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
                            let mut vec = vec![LPrimitive::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(Arc::new(vec))
                        };
                        let result: PLValue = p_expand(&exp, top_level, p_env).await?;
                        if result.is_pure() {
                            return Ok(PLValue::unpure(lv.clone()));
                        }
                        return Ok(PLValue::pure(list![
                            LPrimitive::DefLambda.into(),
                            vars.clone(),
                            result.lvalue_as_quote()
                        ]));
                    }
                    LPrimitive::If => {
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
                        let mut expanded_list = vec![LPrimitive::If.into()];
                        for x in &list[1..] {
                            let result = p_expand(x, false, p_env).await?;
                            if !result.is_pure() {
                                return Ok(PLValue::unpure(x.clone()));
                            }
                            expanded_list.push(result.lvalue)
                        }
                        return Ok(PLValue::pure(expanded_list.into()));
                    }
                    LPrimitive::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                P_EXPAND,
                                list.as_slice(),
                                2..2,
                            ));
                        }
                        return Ok(PLValue::pure(
                            vec![LPrimitive::Quote.into(), list[1].clone()].into(),
                        ));
                    }
                    LPrimitive::Begin | LPrimitive::Do => {
                        return if list.len() == 1 {
                            Ok(PLValue::pure(LValue::Nil))
                        } else {
                            let mut expanded_list = vec![co.into()];
                            for e in &list[1..] {
                                let result = p_expand(e, top_level, p_env).await?;
                                if !result.is_pure() {
                                    return Ok(PLValue::unpure(lv.clone()));
                                }
                                expanded_list.push(result.lvalue)
                            }
                            Ok(PLValue::pure(expanded_list.into()))
                        }
                    }
                    LPrimitive::QuasiQuote => {
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
                            p_expand(&expand_quasi_quote(&list[1], &p_env.env)?, top_level, p_env)
                                .await
                            //Ok(expanded)
                        };
                    }
                    LPrimitive::UnQuote => {
                        return Err(anyhow!(
                            "unquote must be inside a quasiquote expression".to_string(),
                        )
                        .into())
                    }
                    co => {
                        return if list.len() != 2 {
                            Err(LRuntimeError::wrong_number_of_args(P_EXPAND, list, 2..2))
                        } else {
                            let mut expanded = vec![co.into()];
                            let result = p_expand(&list[1], top_level, p_env).await?;

                            if !result.is_pure() {
                                return Ok(PLValue::unpure(lv.clone()));
                            }

                            expanded.push(result.lvalue);
                            Ok(PLValue::pure(expanded.into()))
                        }
                    }
                }
            } else if let LValue::Symbol(sym) = &list[0] {
                match p_env.env.get_macro(sym) {
                    None => {}
                    Some(m) => {
                        let new_env = m.get_new_env(p_env.env.clone(), &list[1..])?;
                        let mut new_p_env = p_env.clone();
                        new_p_env.env = new_env;
                        let result = p_eval(m.get_body(), &mut new_p_env).await?;
                        /*if !result.is_pure() {
                            return Ok(PLValue::unpure(lv.clone()));
                        }*/

                        let expanded = p_expand(&result, top_level, p_env).await?;
                        /*p_env
                        .env
                        .log
                        .trace(format!("In expand: macro expanded: {:?}", expanded));*/
                        return Ok(expanded);
                    }
                }
            }

            let mut expanded_list: Vec<LValue> = vec![];
            for e in list.iter() {
                let result = p_expand(e, false, p_env).await?;
                if result.is_pure() {
                    expanded_list.push(result.lvalue);
                } else {
                    return Ok(PLValue::unpure(lv.clone()));
                }
            }

            Ok(PLValue::pure(expanded_list.into()))
        }
        lv => Ok(PLValue::pure(lv.clone())),
    }
}

pub async fn p_parse(str: &str, env: &mut PLEnv) -> lruntimeerror::Result<PLValue> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => p_expand(&parse_into_lvalue(&se), true, env).await,
        Err(e) => Err(lruntimeerror!(P_PARSE, format!("Error in command: {}", e))),
    }
}
