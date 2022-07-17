extern crate core;

use crate::modules::CtxRoot;
use anyhow::anyhow;
use aries_planning::parsing::sexpr::SExpr;
use async_recursion::async_recursion;
use lazy_static::lazy_static;
use sompas_language::*;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};

use crate::stack_eval::{
    BeginFrame, CoreOperatorFrame, DefineFrame, DoFrame, EvalStack, IfFrame, Interruptibility,
    ProcedureFrame, Results, ScopeCollection, StackFrame, StackKind,
};
use futures::FutureExt;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lswitch::{new_interruption_handler, InterruptSignal, InterruptionReceiver};
use sompas_structs::lvalue::LValue;
use sompas_structs::{string, symbol, wrong_n_args, wrong_type};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fmt::Write;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

pub mod modules;
pub mod stack_eval;
pub mod static_eval;
pub mod test_utils;
lazy_static! {
    ///Global variable used to enable debug println.
    /// Mainly used during development.
    static ref DEBUG: AtomicBool = AtomicBool::new(false);
}

pub async fn get_root_env() -> LEnv {
    // let map = im::hashmap::HashMap::new();
    // map.ins
    let mut env = LEnv::default();
    env.import(CtxRoot::default(), ImportType::WithoutPrefix);
    eval_init(&mut env).await;
    env
}

pub async fn eval_init(env: &mut LEnv) {
    let mut errors = vec![];

    let inner = env.get_init().clone();
    let inner = inner.inner();
    //println!("eval init");
    for e in inner {
        //println!("-{}", e);
        let lv = match parse(e, env).await {
            Ok(lv) => lv,
            Err(e) => {
                errors.push(e);
                continue;
            }
        };
        if let Err(e) = eval(&lv, env, None).await {
            errors.push(e)
        }
    }

    if !errors.is_empty() {
        let mut str = String::new();
        for e in errors {
            writeln!(str, "{}", e).expect("panic at writeln");
        }
        panic!("Errors loading libraries:\n {}", str);
    }
}
//}

/// Enables debugging
/// DEBUG <- true
pub fn activate_debug() {
    DEBUG.store(true, Ordering::Relaxed);
}

/// Returns the value of debug
pub fn get_debug() -> bool {
    DEBUG.load(Ordering::Relaxed)
}

/// Parse an str and returns an expanded LValue
pub async fn parse(str: &str, env: &mut LEnv) -> LResult {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => expand(&parse_into_lvalue(&se), true, env).await,
        Err(e) => Err(anyhow!("Error in command: {}", e.to_string()).into()),
    }
}

/// Transform literals into LValue of types Symbol, Float, Integer or Boolean
pub fn parse_into_lvalue(se: &SExpr) -> LValue {
    match se {
        SExpr::Atom(atom) => {
            return match atom.canonical_str().parse::<i64>() {
                Ok(int) => LValue::Number(LNumber::Int(int)),
                Err(_) => match atom.canonical_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => LValue::Number(LNumber::Float(float)),
                    Err(_) => match atom.canonical_str() {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            LValue::True
                        }
                        FALSE | NIL => {
                            //println!("atom is boolean false");
                            LValue::Nil
                        }
                        s => {
                            if s.starts_with('\"') && s.ends_with('\"') {
                                string!(s[1..s.len() - 1].to_string())
                            } else {
                                symbol!(s.to_string())
                            }
                        }
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let list_iter = list.iter();
            if list_iter.is_empty() {
                LValue::Nil
            } else {
                let vec: Vec<LValue> = list_iter.map(parse_into_lvalue).collect();
                //Expand possible quotting
                //expand_quoting(vec)
                vec.into()
            }
        }
    }
}

/// Expand LValues Expressions as Macros
#[async_recursion]
pub async fn expand(x: &LValue, top_level: bool, env: &mut LEnv) -> LResult {
    match x {
        LValue::List(list) => {
            if let Ok(co) = LCoreOperator::try_from(&list[0]) {
                match co {
                    LCoreOperator::Define | LCoreOperator::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND,
                                list.as_slice(),
                                3..usize::MAX,
                            ));
                        }
                        let def = LCoreOperator::try_from(&list[0])?;
                        let v = &list[1];
                        let body = &list[2..];
                        match v {
                            LValue::List(v_list) => {
                                if v_list.len() >= 2 {
                                    let f = &v_list[0];
                                    let args = &v_list[1..];
                                    let mut new_body = vec![LCoreOperator::DefLambda.into()];
                                    new_body.append(&mut args.to_vec());
                                    new_body.append(&mut body.to_vec());
                                    return expand(
                                        &vec![def.into(), f.clone(), new_body.into()].into(),
                                        top_level,
                                        env,
                                    )
                                    .await;
                                }
                            }
                            LValue::Symbol(sym) => {
                                if list.len() != 3 {
                                    return Err(wrong_n_args!("expand", list, 3));
                                }
                                let exp = expand(&list[2], top_level, env).await?;
                                //println!("after expansion: {}", exp);
                                if def == LCoreOperator::DefMacro {
                                    if !top_level {
                                        return Err(anyhow!(
                                            "{}: defmacro only allowed at top level",
                                            x
                                        )
                                        .into());
                                    }
                                    let proc = eval(&exp, env, None).await?;
                                    //println!("new macro: {}", proc);
                                    if !matches!(proc, LValue::Lambda(_)) {
                                        return Err(
                                            anyhow!("{}: macro must be a procedure", proc).into()
                                        );
                                    } else {
                                        env.add_macro(sym.to_string(), proc.try_into()?);
                                    }
                                    //println!("macro added");
                                    //Add to macro_table
                                    return Ok(LValue::Nil);
                                }
                                //We add to the list the expanded body
                                return Ok(
                                    vec![LCoreOperator::Define.into(), v.clone(), exp].into()
                                );
                            }
                            _ => return Err(wrong_type!("expand", x, KindLValue::Symbol)),
                        }
                    }
                    LCoreOperator::DefLambda => {
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                "expand",
                                list,
                                3..std::usize::MAX,
                            ));
                        }
                        let vars = &list[1];
                        let body = &list[2..];
                        //Verification of the types of the arguments
                        match vars {
                            LValue::List(vars_list) => {
                                for v in vars_list.iter() {
                                    if !matches!(v, LValue::Symbol(_)) {
                                        return Err(
                                            anyhow!("illegal lambda argument list: {}", x).into()
                                        );
                                    }
                                }
                            }
                            LValue::Symbol(_) | LValue::Nil => {}
                            lv => {
                                return Err(LRuntimeError::not_in_list_of_expected_types(
                                    "expand",
                                    lv,
                                    vec![KindLValue::List, KindLValue::Symbol],
                                ))
                            }
                        }
                        let exp = if body.len() == 1 {
                            body[0].clone()
                        } else {
                            let mut vec = vec![LCoreOperator::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(Arc::new(vec))
                        };
                        return Ok(vec![
                            LCoreOperator::DefLambda.into(),
                            vars.clone(),
                            expand(&exp, top_level, env).await?,
                        ]
                        .into());
                    }
                    LCoreOperator::If => {
                        let mut list = list.deref().clone();
                        if list.len() == 3 {
                            list.push(LValue::Nil);
                        }
                        if list.len() != 4 {
                            return Err(wrong_n_args!("expand", list.as_slice(), 4));
                        }
                        //return map(expand, x)
                        let mut expanded_list = vec![LCoreOperator::If.into()];
                        for x in &list[1..] {
                            expanded_list.push(expand(x, false, env).await?)
                        }
                        return Ok(expanded_list.into());
                    }
                    LCoreOperator::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(wrong_n_args!("expand", list.as_slice(), 2));
                        }
                        return Ok(vec![LCoreOperator::Quote.into(), list[1].clone()].into());
                    }
                    LCoreOperator::Begin | LCoreOperator::Do => {
                        return if list.len() == 1 {
                            Ok(LValue::Nil)
                        } else {
                            let mut expanded_list = vec![co.into()];
                            for x in &list[1..] {
                                expanded_list.push(expand(x, top_level, env).await?)
                            }
                            Ok(expanded_list.into())
                        }
                    }
                    LCoreOperator::QuasiQuote => {
                        return if list.len() != 2 {
                            return Err(wrong_n_args!("expand", list, 2));
                        } else {
                            expand(&expand_quasi_quote(&list[1], env)?, top_level, env).await
                        };
                    }
                    LCoreOperator::UnQuote => {
                        return Err(anyhow!("unquote must be inside a quasiquote expression").into())
                    }
                    LCoreOperator::Async => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list.as_slice(), 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Async.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Await => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list.as_slice(), 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Await.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Eval => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Eval.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Parse => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Parse.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Expand => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Expand.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Interrupt => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Interrupt.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Interruptible => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Interruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::Uninterruptible => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::Uninterruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LCoreOperator::QuasiInterruptible => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LCoreOperator::QuasiInterruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                }
            } else if let LValue::Symbol(sym) = &list[0] {
                match env.get_macro(sym) {
                    None => {}
                    Some(m) => {
                        let lv = m.get_body();
                        let env = &mut m.get_new_env(env.clone(), &list[1..])?;

                        let expanded = expand(&eval(&lv, env, None).await?, top_level, env).await?;
                        if get_debug() {
                            println!("In expand: macro expanded: {:?}", expanded);
                        }
                        return Ok(expanded);
                    }
                }
            }

            let mut expanded_list: Vec<LValue> = vec![];
            for e in list.iter() {
                expanded_list.push(expand(e, false, env).await?);
            }

            Ok(expanded_list.into())
        }
        lv => Ok(lv.clone()),
    }
}

/// Expand quasiquote expressions
pub fn expand_quasi_quote(x: &LValue, env: &LEnv) -> LResult {
    match x {
        LValue::List(list) => {
            if list.is_empty() {
                Ok(LValue::Nil)
            } else {
                let first = &list[0];
                if let LValue::Symbol(s) = first {
                    if let Ok(co) = LCoreOperator::try_from(s.as_str()) {
                        if co == LCoreOperator::UnQuote {
                            if list.len() != 2 {
                                return Err(wrong_n_args!("expand_quasi_quote", list, 2));
                            }
                            return Ok(list[1].clone());
                        }
                    }
                }
                Ok(vec![
                    env.get_symbol(CONS)
                        .expect("problem in the definition of the root env"),
                    expand_quasi_quote(first, env)?,
                    expand_quasi_quote(&list[1..].to_vec().into(), env)?,
                ]
                .into())
            }
        }
        _ => Ok(vec![LCoreOperator::Quote.into(), x.clone()].into()),
    }
    //Verify if has unquotesplicing here
}

/// Evaluate a LValue
/// Main function of the Scheme Interpreter
#[async_recursion]
pub async fn eval(
    lv: &LValue,
    root_env: &mut LEnv,
    mut int: Option<InterruptionReceiver>,
) -> LResult {
    let mut interrupted = InterruptSignal::NInterrupted;
    let error = LValue::Err(LValue::from("interrupted").into());
    let mut queue: EvalStack = Default::default();
    queue.push(StackFrame::interruptible(lv.clone()));

    let mut scopes: ScopeCollection = ScopeCollection::new(root_env);
    let mut results: Results = Default::default();

    loop {
        if let Some(r) = &mut int {
            interrupted = r.is_interrupted();
        }

        let current = match queue.pop() {
            Some(lv) => lv,
            None => break,
        };

        let interruptibility = current.interruptibily;

        if interrupted == InterruptSignal::Interrupted
            && interruptibility == Interruptibility::Interruptible
        {
            match current.kind {
                StackKind::NonEvaluated(_) => {
                    //
                }
                StackKind::Procedure(p) => {
                    results.pop_n(p.n);
                    scopes.revert_scope();
                }
                StackKind::CoreOperator(co) => match co {
                    CoreOperatorFrame::If(_) => {
                        results.pop();
                        scopes.revert_scope();
                    }
                    CoreOperatorFrame::Begin(b) => {
                        let mut r = results.pop_n(b.n);
                        scopes.revert_scope();
                        results.push(r.pop().unwrap());
                    }
                    CoreOperatorFrame::Do(d) => {
                        scopes.revert_scope();
                    }
                    CoreOperatorFrame::Define(d) => {
                        results.pop();
                    }
                    CoreOperatorFrame::Lambda => {
                        scopes.revert_scope();
                        scopes.revert_scope();
                    }
                    CoreOperatorFrame::Await => {
                        results.pop();
                        results.push(error.clone());
                    }
                    CoreOperatorFrame::Interrupt => {
                        results.pop();
                        results.push(error.clone());
                    }
                    CoreOperatorFrame::Eval => {
                        results.pop();
                        results.push(error.clone());
                    }
                    CoreOperatorFrame::Expand => {
                        results.pop();
                        results.push(error.clone());
                    }
                    CoreOperatorFrame::Parse => {
                        results.pop();
                        results.push(error.clone());
                    }
                },
            }
            results.push(error.clone());
            continue;
        }

        match current.kind {
            StackKind::NonEvaluated(lv) => match lv {
                LValue::Symbol(s) => {
                    let result = match scopes.get_last().get_symbol(s.as_str()) {
                        None => s.into(),
                        Some(lv) => lv,
                    };
                    results.push(result);
                }
                LValue::List(list) => {
                    let list = list.as_slice();
                    let proc = &list[0];
                    let args = &list[1..];
                    if let LValue::CoreOperator(co) = proc {
                        match co {
                            LCoreOperator::Define => {
                                match &args[0] {
                                    LValue::Symbol(s) => {
                                        queue.push(StackFrame::new(
                                            DefineFrame { symbol: s.clone() },
                                            interruptibility,
                                        ));
                                        queue.push(StackFrame::new(
                                            args[1].clone(),
                                            interruptibility,
                                        ));
                                    }
                                    lv => return Err(wrong_type!("eval", lv, KindLValue::Symbol)),
                                };
                            }
                            LCoreOperator::DefLambda => {
                                let params = match &args[0] {
                                    LValue::List(list) => {
                                        let mut vec_sym = Vec::new();
                                        for val in list.iter() {
                                            match val {
                                                LValue::Symbol(s) => vec_sym.push(s.clone()),
                                                lv => {
                                                    return Err(wrong_type!(
                                                        "eval",
                                                        &lv,
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
                                            EVAL,
                                            lv,
                                            vec![KindLValue::List, KindLValue::Symbol],
                                        ))
                                    }
                                };
                                let body = &args[1];
                                results.push(LValue::Lambda(LLambda::new(
                                    params,
                                    body.clone(),
                                    scopes.get_last().get_symbols(),
                                )));
                            }
                            LCoreOperator::If => {
                                scopes.new_scope();
                                let stack = IfFrame {
                                    conseq: args[1].clone(),
                                    alt: args[2].clone(),
                                };

                                queue.push(StackFrame::new(stack, interruptibility));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Quote => results.push(args[0].clone()),
                            LCoreOperator::QuasiQuote => {
                                panic!("quasiquote not allowed")
                            }
                            LCoreOperator::UnQuote => {
                                panic!("unquote not allowed")
                            }
                            LCoreOperator::DefMacro => {
                                panic!("defmacro not allowed")
                            }
                            LCoreOperator::Begin => {
                                scopes.new_scope();
                                let stack = BeginFrame { n: args.len() };
                                queue.push(StackFrame::new(stack, interruptibility));
                                queue.push_list(
                                    args.iter()
                                        .map(|a| StackFrame::new(a.clone(), interruptibility))
                                        .collect(),
                                );
                            }
                            LCoreOperator::Do => {
                                scopes.new_scope();
                                let stack = DoFrame {
                                    rest: args[1..].to_vec(),
                                };

                                queue.push(StackFrame::new(stack, interruptibility));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Async => {
                                let lvalue = args[0].clone();
                                let mut new_env = scopes.get_last().clone();

                                /*let future: LValue =
                                tokio::spawn(
                                    async move { eval(&lvalue, &mut new_env, &mut ctxs).await },
                                )
                                .await
                                .unwrap()?;*/

                                let (tx, rx) = new_interruption_handler();

                                let future: FutureResult =
                                    Box::pin(
                                        async move { eval(&lvalue, &mut new_env, Some(rx)).await },
                                    ) as FutureResult;
                                let future: LFuture = future.shared();
                                let future_2 = future.clone();
                                tokio::spawn(async move {
                                    #[allow(unused_must_use)]
                                    future_2.await
                                });

                                let handler = LAsyncHandler::new(future, tx);

                                results.push(handler.into());
                            }
                            LCoreOperator::Await => {
                                //println!("awaiting on async evaluation");
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Await,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Eval => {
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Eval,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Expand,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Expand => {
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Expand,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Parse => {
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Parse,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }

                            LCoreOperator::Interrupt => {
                                queue.push(StackFrame::new(
                                    CoreOperatorFrame::Interrupt,
                                    interruptibility,
                                ));
                                queue.push(StackFrame::new(args[0].clone(), interruptibility));
                            }
                            LCoreOperator::Interruptible => {
                                if interruptibility != Interruptibility::Unininterruptible {
                                    queue.push(StackFrame::interruptible(args[0].clone()))
                                } else {
                                    return Err(LRuntimeError::new(
                                        EVAL,
                                        "i! not allowed outside i?",
                                    ));
                                }
                            }
                            LCoreOperator::Uninterruptible => {
                                queue.push(StackFrame::uninterruptible(args[0].clone()))
                            }
                            LCoreOperator::QuasiInterruptible => {
                                queue.push(StackFrame::quasiinterruptible(args[0].clone()))
                            }
                        }
                    } else {
                        scopes.new_scope();
                        queue.push(StackFrame::new(
                            ProcedureFrame { n: list.len() },
                            interruptibility,
                        ));
                        queue.push_list(
                            list.iter()
                                .map(|a| StackFrame::new(a.clone(), interruptibility))
                                .collect(),
                        );
                    }
                }
                _ => results.push(lv),
            },
            StackKind::Procedure(pro) => {
                let exps: Vec<LValue> = results.pop_n(pro.n);
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        queue.push(StackFrame::new(CoreOperatorFrame::Lambda, interruptibility));
                        queue.push(StackFrame::new(l.get_body().clone(), interruptibility));
                        let temp_env = l.get_new_env(scopes.get_last().clone(), args)?;
                        scopes.new_defined_scope(temp_env);
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = fun.call(scopes.get_last(), args)?;
                        results.push(r_lvalue);
                        scopes.revert_scope();
                    }
                    LValue::AsyncFn(fun) => {
                        let r_lvalue = fun.call(scopes.get_last(), args).await?;
                        results.push(r_lvalue);
                        scopes.revert_scope();
                    }
                    lv => {
                        return Err(wrong_type!("eval", lv, KindLValue::Fn));
                    }
                }
            }
            StackKind::CoreOperator(cos) => match cos {
                CoreOperatorFrame::Define(d) => {
                    let env = scopes.get_last_mut();
                    let value = results.pop().unwrap();
                    env.insert(d.symbol.as_ref(), value);
                    results.push(LValue::Nil);
                }
                CoreOperatorFrame::If(i) => {
                    let result = results.pop().unwrap();

                    match result {
                        LValue::True => queue.push(StackFrame::new(i.conseq, interruptibility)),
                        LValue::Nil => queue.push(StackFrame::new(i.alt, interruptibility)),
                        lv => return Err(wrong_type!("eval", &lv, KindLValue::Bool)),
                    };
                    scopes.revert_scope()
                }
                CoreOperatorFrame::Do(mut df) => {
                    if df.rest.is_empty() {
                        scopes.revert_scope();
                    } else {
                        let result = results.pop().unwrap();
                        if matches!(result, LValue::Err(_)) {
                            results.push(result);
                            scopes.revert_scope();
                        } else {
                            let next = df.rest.remove(0);
                            queue.push(StackFrame::new(df, interruptibility));
                            queue.push(StackFrame::new(next, interruptibility));
                        }
                    }
                }
                CoreOperatorFrame::Begin(b) => {
                    let mut r = results.pop_n(b.n);
                    results.push(r.pop().unwrap());
                    scopes.revert_scope();
                }
                CoreOperatorFrame::Lambda => {
                    scopes.revert_scope();
                    scopes.revert_scope();
                }
                CoreOperatorFrame::Await => {
                    let h = results.pop().unwrap();
                    if let LValue::Handler(h) = h {
                        let f = h.get_future();

                        match f.await {
                            Err(e) => return Err(e),
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        return Err(wrong_type!(EVAL, &h, KindLValue::Handler));
                    };
                }
                CoreOperatorFrame::Interrupt => {
                    let mut h = results.pop().unwrap();
                    if let LValue::Handler(mut h) = h {
                        match h.interrupt().await {
                            Err(e) => return Err(e),
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        return Err(wrong_type!(EVAL, &h, KindLValue::Handler));
                    };
                }
                CoreOperatorFrame::Eval => {
                    queue.push(StackFrame::new(results.pop().unwrap(), interruptibility));
                }
                CoreOperatorFrame::Expand => {
                    let result = results.pop().unwrap();
                    results.push(expand(&result, true, scopes.get_last_mut()).await?);
                }
                CoreOperatorFrame::Parse => {
                    let result = results.pop().unwrap();
                    if let LValue::String(s) = result {
                        results.push(parse(s.as_str(), scopes.get_last_mut()).await?)
                    } else {
                        return Err(wrong_type!("eval", &result, KindLValue::String));
                    };
                }
            },
        }
    }
    Ok(results.pop().unwrap())
}

/*#[async_recursion]
pub async fn eval(lv: &LValue, env: &mut LEnv) -> LResult {
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
            if get_debug() {
                println!("{} => {}", str, result)
            }
            return Ok(result);
        } else if let LValue::List(list) = &lv {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            if let LValue::CoreOperator(co) = proc {
                match co {
                    LCoreOperator::Define => {
                        match &args[0] {
                            LValue::Symbol(s) => {
                                let exp = eval(&args[1], env).await?;
                                env.insert(s.to_string(), exp);
                            }
                            lv => return Err(wrong_type!("eval", lv, KindLValue::Symbol)),
                        };
                        if get_debug() {
                            println!("{} => {}", str, LValue::Nil);
                        }
                        return Ok(LValue::Nil);
                    }
                    LCoreOperator::DefLambda => {
                        //println!("it is a lambda");
                        let params = match &args[0] {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list.iter() {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv => {
                                            return Err(wrong_type!(
                                                "eval",
                                                &lv,
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
                                    "eval",
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
                        return Ok(r_lvalue);
                    }
                    LCoreOperator::If => {
                        let test = &args[0];
                        let conseq = &args[1];
                        let alt = &args[2];
                        lv = match eval(test, env).await? {
                            LValue::True => conseq.clone(),
                            LValue::Nil => alt.clone(),
                            lv => return Err(wrong_type!("eval", &lv, KindLValue::Bool)),
                        };
                    }
                    LCoreOperator::Quote => {
                        if get_debug() {
                            println!("{} => {}", str, &args[0].clone());
                        }
                        return Ok(args[0].clone());
                    }
                    LCoreOperator::Begin | LCoreOperator::Do => {
                        let _do = *co == LCoreOperator::Do;
                        let firsts = &args[0..args.len() - 1];
                        let last = args.last().unwrap();

                        for e in firsts {
                            let result: LValue = eval(e, env).await?;

                            if _do && matches!(result, LValue::Err(_)) {
                                return Ok(result);
                            }
                        }
                        lv = last.clone();
                    }
                    LCoreOperator::QuasiQuote
                    | LCoreOperator::UnQuote
                    | LCoreOperator::DefMacro => return Ok(LValue::Nil),
                    /*LCoreOperator::Async => {
                        //println!("async evaluation");
                        let lvalue = args[0].clone();
                        let mut new_env = env.clone();

                        /*let future: LValue =
                        tokio::spawn(
                            async move { eval(&lvalue, &mut new_env, &mut ctxs).await },
                        )
                        .await
                        .unwrap()?;*/

                        let future: LValue =
                            (Box::pin(async move { eval(&lvalue, &mut new_env).await })
                                as FutureResult)
                                .into();
                        let future_2 = future.clone();
                        tokio::spawn(async move {
                            #[allow(unused_must_use)]
                            if let LValue::Future(future_2) = future_2 {
                                future_2.await;
                            }
                        });

                        return Ok(future);
                    }
                    LCoreOperator::Await => {
                        //println!("awaiting on async evaluation");
                        let future = eval(&args[0], env).await?;

                        return if let LValue::Future(future) = future {
                            future.await
                        } else {
                            return Err(wrong_type!(EVAL, &future, KindLValue::Future));
                        };
                    }*/
                    LCoreOperator::Eval => {
                        let arg = &args[0];
                        lv = expand(&eval(arg, env).await?, true, env).await?;
                    }
                    LCoreOperator::Parse => {
                        let result = eval(&args[0], env).await?;
                        return if let LValue::String(s) = result {
                            parse(s.as_str(), env).await
                        } else {
                            return Err(wrong_type!("eval", &result, KindLValue::String));
                        };
                    }
                    LCoreOperator::Expand => {
                        let arg = &args[0];
                        return expand(&eval(arg, env).await?, true, env).await;
                    }
                    co => panic!("{} not yet supported", co),
                }
            } else {
                let mut exps: Vec<LValue> = vec![];

                for x in list {
                    exps.push(eval(x, env).await?)
                }

                /*let exps = list
                .iter()
                .map(|x| eval(x, &mut env, ctxs).await)
                .collect::<Result<Vec<LValue>, _>>()?;*/
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        lv = l.get_body().clone();
                        temp_env = l.get_new_env(env.clone(), args)?;
                        env = &mut temp_env;
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = fun.call(env, args)?;
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        return Ok(r_lvalue);
                    }
                    LValue::AsyncFn(fun) => {
                        let r_lvalue = fun.call(env, args).await?;
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        return Ok(r_lvalue);
                    }
                    lv => {
                        return Err(wrong_type!("eval", lv, KindLValue::Fn));
                    }
                };
            }
        } else {
            if get_debug() {
                println!("{} => {}", str, lv.clone());
            }
            return Ok(lv);
        }
    }
}*/

/*/// Evaluate a LValue
/// Main function of the Scheme Interpreter
#[async_recursion]
pub async fn eval_interruptible(
    lv: &LValue,
    env: &mut LEnv,
    ir: &mut InterruptionReceiver,
    i: Interruptibility,
) -> LResult {
    let mut lv = lv.clone();
    let eval_result: LResult;
    let mut temp_env: LEnv;
    let mut env = env;

    let str = format!("{}", lv);

    loop {
        if ir.is_interrupted() {
            interrupted = true;
            lv = interrupt_handler.clone();
        }

        if let LValue::Symbol(s) = &lv {
            let result = match env.get_symbol(s.as_str()) {
                None => lv.clone(),
                Some(lv) => lv,
            };
            if get_debug() {
                println!("{} => {}", str, result)
            }
            eval_result = Ok(result);
            break;
        } else if let LValue::List(list) = &lv {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            if let LValue::CoreOperator(co) = proc {
                match co {
                    LCoreOperator::Define => {
                        match &args[0] {
                            LValue::Symbol(s) => {
                                let exp = eval(&args[1], env).await?;
                                env.insert(s.to_string(), exp);
                            }
                            lv => return Err(wrong_type!("eval", lv, KindLValue::Symbol)),
                        };
                        if get_debug() {
                            println!("{} => {}", str, LValue::Nil);
                        }
                        eval_result = Ok(LValue::Nil);
                        break;
                    }
                    LCoreOperator::DefLambda => {
                        //println!("it is a lambda");
                        let params = match &args[0] {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list.iter() {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv => {
                                            return Err(wrong_type!(
                                                "eval",
                                                &lv,
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
                                    "eval",
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
                        eval_result = Ok(r_lvalue);
                        break;
                    }
                    LCoreOperator::If => {
                        let test = &args[0];
                        let conseq = &args[1];
                        let alt = &args[2];
                        lv = match eval(test, env).await? {
                            LValue::True => conseq.clone(),
                            LValue::Nil => alt.clone(),
                            lv => {
                                eval_result = Err(wrong_type!("eval", &lv, KindLValue::Bool));
                                break;
                            }
                        };
                    }
                    LCoreOperator::Quote => {
                        if get_debug() {
                            println!("{} => {}", str, &args[0].clone());
                        }
                        eval_result = Ok(args[0].clone());
                        break;
                    }
                    LCoreOperator::Begin | LCoreOperator::Do => {
                        let _do = *co == LCoreOperator::Do;
                        let firsts = &args[0..args.len() - 1];
                        let last = args.last().unwrap();

                        for e in firsts {
                            let result: LValue = eval(e, env).await?;

                            if _do && matches!(result, LValue::Err(_)) {
                                eval_result = Ok(result);
                                break;
                            }
                        }
                        lv = last.clone();
                    }
                    LCoreOperator::QuasiQuote
                    | LCoreOperator::UnQuote
                    | LCoreOperator::DefMacro => {
                        return Err(LRuntimeError::new(
                            EVAL,
                            format!(
                                "{}, {} and {} should not be present in evaluated expression.",
                                QUASI_QUOTE, UNQUOTE, DEF_MACRO
                            ),
                        ))
                    }
                    LCoreOperator::Async => {
                        //println!("async evaluation");
                        let lvalue = args[0].clone();
                        let mut new_env = env.clone();

                        let future: LValue =
                            (Box::pin(async move { eval(&lvalue, &mut new_env).await })
                                as FutureResult)
                                .into();
                        let future_2 = future.clone();
                        tokio::spawn(async move {
                            #[allow(unused_must_use)]
                            if let LValue::Future(future_2) = future_2 {
                                future_2.await;
                            }
                        });

                        eval_result = Ok(future);
                        break;
                    }
                    LCoreOperator::Await => {
                        //println!("awaiting on async evaluation");
                        let future = eval(&args[0], env).await?;

                        return if let LValue::Future(future) = future {
                            future.await
                        } else {
                            eval_result = Err(wrong_type!(EVAL, &future, KindLValue::Future));
                            break;
                        };
                    }
                    LCoreOperator::Eval => {
                        let arg = &args[0];
                        lv = expand(&eval(arg, env).await?, true, env).await?;
                    }
                    LCoreOperator::Parse => {
                        let result = eval(&args[0], env).await?;
                        return if let LValue::String(s) = result {
                            parse(s.as_str(), env).await
                        } else {
                            eval_result = Err(wrong_type!("eval", &result, KindLValue::String));
                            break;
                        };
                    }
                    LCoreOperator::Expand => {
                        let arg = &args[0];
                        eval_result = expand(&eval(arg, env).await?, true, env).await;
                        break;
                    }
                    LCoreOperator::Interrupt => {}
                    LCoreOperator::Interruptible => {}
                    LCoreOperator::Uninterruptible => {}
                    LCoreOperator::QuasiInterruptible => {}
                }
            } else {
                let mut exps: Vec<LValue> = vec![];

                for x in list {
                    exps.push(eval(x, env).await?)
                }

                /*let exps = list
                .iter()
                .map(|x| eval(x, &mut env, ctxs).await)
                .collect::<Result<Vec<LValue>, _>>()?;*/
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        lv = l.get_body().clone();
                        temp_env = l.get_new_env(env.clone(), args)?;
                        env = &mut temp_env;
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = fun.call(env, args)?;
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        eval_result = Ok(r_lvalue);
                        break;
                    }
                    LValue::AsyncFn(fun) => {
                        let r_lvalue = fun.call(env, args).await?;
                        if get_debug() {
                            println!("{} => {}", str, r_lvalue);
                        }
                        eval_result = Ok(r_lvalue);
                        break;
                    }
                    lv => {
                        eval_result = Err(wrong_type!("eval", lv, KindLValue::Fn));
                        break;
                    }
                };
            }
        } else {
            if get_debug() {
                println!("{} => {}", str, lv.clone());
            }
            eval_result = Ok(lv);
            break;
        }
    }

    if interrupted {
        if let Ok(lv) = &eval_result {
            Ok(LValue::Err(lv.into_ref()))
        } else {
            eval_result
        }
    } else {
        eval_result
    }
}

#[inline]
pub fn eval_async(lv: &LValue, env: &mut LEnv) -> LResult {
    let future: LFuture = (Box::pin(async move { eval(lv, env).await }) as FutureResult).shared();
    let future_2 = future.clone();
    tokio::spawn(async move {
        #[allow(unused_must_use)]
        if let LValue::Future(future_2) = future_2 {
            future_2.await;
        }
    });

    let (tx, rx) = new_interruption_handler();

    let async_handler: LAsyncHandler = LAsyncHandler::new(future, tx);

    Ok(async_handler.into())
}

/*
pub fn eval_non_recursive(lv: &LValue, env: &mut LEnv) -> lerror::Result<LValue> {
    if let LValue::Symbol(s) = &lv {
        let result = match env.get_symbol(s.as_str()) {
            None => lv.clone(),
            Some(lv) => lv,
        };
        if get_debug() {
            println!("{} => {}", str, result)
        }
        return Ok(result);
    } else if let LValue::List(list) = &lv {
        //println!("expression is a list");
        let list = list.as_slice();
        let proc = &list[0];
        let args = &list[1..];
        //assert!(args.len() >= 2, "Checked in expansion");
        if let LValue::CoreOperator(co) = proc {
            match co {
                LCoreOperator::Define => {
                    match &args[0] {
                        LValue::Symbol(s) => {
                            env.insert(s.to_string(), args[1].clone());
                        }
                        lv => {
                            return Err(wrong_type!(
                                "eval",
                                lv.clone(),
                                lv.into(),
                                KindLValue::Symbol,
                            ))
                        }
                    };
                    if get_debug() {
                        println!("{} => {}", str, LValue::Nil);
                    }
                    return Ok(LValue::Nil);
                }
                LCoreOperator::DefLambda => {
                    //println!("it is a lambda");
                    let params = match &args[0] {
                        LValue::List(list) => {
                            let mut vec_sym = Vec::new();
                            for val in list {
                                match val {
                                    LValue::Symbol(s) => vec_sym.push(s.clone()),
                                    lv => {
                                        return Err(wrong_type!(
                                            "eval",
                                            lv.clone(),
                                            lv.into(),
                                            KindLValue::Symbol,
                                        ))
                                    }
                                }
                            }
                            vec_sym.into()
                        }
                        LValue::Symbol(s) => s.clone().into(),
                        LValue::Nil => LambdaArgs::Nil,
                        lv => {
                            return Err(NotInListOfExpectedTypes(
                                "eval",
                                lv.clone(),
                                lv.into(),
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
                    Ok(r_lvalue)
                }
                LCoreOperator::If => {
                    let test = &args[0];
                    let conseq = &args[1];
                    let alt = &args[2];
                    match &args[0] {
                        LValue::True => Ok(conseq.clone()),
                        LValue::Nil => Ok(alt.clone()),
                        lv => {
                            return Err(wrong_type!("eval", lv.clone(), lv.into(), KindLValue::Bool))
                        }
                    }
                }
                LCoreOperator::Quote => {
                    if get_debug() {
                        println!("{} => {}", str, &args[0].clone());
                    }
                    Ok(args[0].clone())
                }
                LCoreOperator::Begin => {
                    let firsts = &args[0..args.len() - 1];
                    let last = args.last().unwrap();

                    for e in firsts {
                        eval(e, env).await?;
                    }
                    Ok(last.clone())
                }
                LCoreOperator::QuasiQuote | LCoreOperator::UnQuote | LCoreOperator::DefMacro => {
                    return Ok(LValue::Nil)
                }
                LCoreOperator::Eval => {
                    let arg = &args[0];
                    expand(&eval(arg, env).await?, true, env).await
                }
                LCoreOperator::Parse => {
                    if let LValue::String(s) = eval(&args[0], env).await? {
                        parse(s.as_str(), env).await
                    } else {
                        return Err(wrong_type!(
                            "eval",
                            args[0].clone(),
                            (&args[0]).into(),
                            KindLValue::String,
                        ));
                    }
                }
                LCoreOperator::Expand => {
                    let arg = &args[0];
                    expand(&eval(arg, env).await?, true, env).await
                }
                LCoreOperator::Async => {}
                LCoreOperator::Await => {}
            }
        } else {
            /*let exps = list
            .iter()
            .map(|x| eval(x, &mut env, ctxs).await)
            .collect::<Result<Vec<LValue>, _>>()?;*/
            let proc = &list[0];
            let args = &list[1..];
            match proc {
                LValue::Lambda(l) => l.call(args, env).await,
                LValue::Fn(fun) => {
                    let r_lvalue = fun.call(args, env)?;
                    if get_debug() {
                        println!("{} => {}", str, r_lvalue);
                    }
                    return Ok(r_lvalue);
                }
                LValue::AsyncFn(fun) => {
                    let r_lvalue = fun.call(args, env).await?;
                    if get_debug() {
                        println!("{} => {}", str, r_lvalue);
                    }
                    return Ok(r_lvalue);
                }
                lv => {
                    return Err(wrong_type!("eval", lv.clone(), lv.into(), KindLValue::Fn));
                }
            };
        }
    } else {
        if get_debug() {
            println!("{} => {}", str, lv.clone());
        }
        return Ok(lv.clone());
    }
}*/*/
