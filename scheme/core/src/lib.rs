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

use crate::structs::{
    BeginFrame, CoreOperatorFrame, DefineFrame, DoFrame, EvalStack, IfFrame, Interruptibility,
    LDebug, ProcedureFrame, Results, ScopeCollection, StackFrame, StackKind, Unstack,
};
use crate::Interruptibility::Unininterruptible;
use futures::FutureExt;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lswitch::{new_interruption_handler, InterruptSignal, InterruptionReceiver};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, string, symbol, wrong_n_args, wrong_type};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fmt::Write;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

pub mod modules;
pub mod static_eval;
pub mod structs;
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
                            Err(wrong_n_args!("expand", list, 2)
                                .chain(format!("{} must have one arg", UNINTERRUPTIBLE)))
                        } else {
                            let mut expanded = vec![LCoreOperator::Uninterruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    } /*LCoreOperator::QuasiInterruptible => {
                    return if list.len() != 2 {
                    Err(wrong_n_args!("expand", list, 2))
                    } else {
                    let mut expanded = vec![LCoreOperator::QuasiInterruptible.into()];
                    expanded.push(expand(&list[1], top_level, env).await?);
                    Ok(expanded.into())
                    }
                    }*/
                    LCoreOperator::Race => {
                        return if list.len() != 3 {
                            Err(wrong_n_args!("expand", list, 3)
                                .chain(format!("{} must have 2 args.", RACE)))
                        } else {
                            let mut expanded = vec![LCoreOperator::Race.into()];
                            for e in &list[1..] {
                                expanded.push(expand(e, top_level, env).await?);
                            }
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

                        let expanded = expand(&eval(lv, env, None).await?, top_level, env).await?;
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

#[inline]
pub fn async_eval(lv: LValue, mut env: LEnv) -> LAsyncHandler {
    let (tx, rx) = new_interruption_handler();

    let future: FutureResult = Box::pin(async move {
        let r = eval(&lv, &mut env, Some(rx)).await;
        r
    }) as FutureResult;
    let future: LFuture = future.shared();
    let future_2 = future.clone();
    tokio::spawn(async move {
        #[allow(unused_must_use)]
        let _ = future_2.await;
        //println!("done");
    });

    LAsyncHandler::new(future, tx)
}

/// Evaluate a LValue
/// Main function of the Scheme Interpreter
#[async_recursion]
pub async fn eval(
    lv: &LValue,
    root_env: &mut LEnv,
    mut int: Option<InterruptionReceiver>,
) -> LResult {
    let mut debug: LDebug = Default::default();
    debug.push(Unininterruptible, lv);

    let mut interrupted = InterruptSignal::NInterrupted;
    let error = LValue::Err(LValue::from(INTERRUPTED).into());
    let mut queue: EvalStack = Default::default();
    queue.push(StackFrame::new_lvalue(
        lv.clone(),
        Interruptibility::Interruptible,
    ));

    let mut scopes: ScopeCollection = ScopeCollection::new(root_env);
    let mut results: Results = Default::default();
    let mut expression_error: LValue = LValue::Nil;

    let result: LResult = loop {
        let current = match queue.pop() {
            Some(lv) => lv,
            None => break Ok(results.pop().unwrap()),
        };

        if let Some(r) = &mut int {
            interrupted = r.is_interrupted();
            if get_debug() && interrupted == InterruptSignal::Interrupted {
                println!("interrupted! last result!: {:?}", results.last())
            }
        }

        let interruptibility = current.interruptibily;

        if interrupted == InterruptSignal::Interrupted
            && interruptibility == Interruptibility::Interruptible
        {
            match current.kind {
                StackKind::NonEvaluated(_) => {
                    results.push(error.clone());
                }
                StackKind::Procedure(p) => {
                    results.pop_n(p.n);
                    scopes.revert_scope();
                    results.push(error.clone());
                }
                StackKind::CoreOperator(co) => match co {
                    CoreOperatorFrame::If(_) => {
                        results.pop();
                        results.push(error.clone());
                        scopes.revert_scope();
                    }
                    CoreOperatorFrame::Begin(b) => {
                        let mut r = results.pop_n(b.n);
                        scopes.revert_scope();
                        results.push(r.pop().unwrap());
                        debug.print_last_result(&results);
                    }
                    CoreOperatorFrame::Do(_) => {
                        scopes.revert_scope();
                    }
                    CoreOperatorFrame::Define(_) => {
                        results.pop();
                        results.push(error.clone());
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
            debug.print_last_result(&results);
            continue;
        } else if interrupted == InterruptSignal::Interrupted
            && current.interruptibily == Unininterruptible
        {
            println!("interrupt avoided");
        }

        match current.kind {
            StackKind::NonEvaluated(ref lv) => {
                if get_debug() {
                    debug.push(current.interruptibily, lv.to_string());
                }
                match lv {
                    LValue::Symbol(s) => {
                        let result = match scopes.get_last().get_symbol(s.as_str()) {
                            None => s.into(),
                            Some(lv) => lv,
                        };
                        results.push(result);
                        debug.print_last_result(&results);
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
                                            queue.push(StackFrame::new_lvalue(
                                                args[1].clone(),
                                                interruptibility,
                                            ));
                                        }
                                        lv => {
                                            let err = Err(wrong_type!(
                                                "eval",
                                                &lv.clone(),
                                                KindLValue::Symbol
                                            ));
                                            expression_error = current.unstack(&mut results);
                                            break err;
                                        }
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
                                                EVAL,
                                                &lv.clone(),
                                                vec![KindLValue::List, KindLValue::Symbol],
                                            );
                                            expression_error = current.unstack(&mut results);

                                            break Err(err);
                                        }
                                    };
                                    let body = &args[1];
                                    results.push(LValue::Lambda(LLambda::new(
                                        params,
                                        body.clone(),
                                        scopes.get_last().get_symbols(),
                                    )));
                                    debug.print_last_result(&results);
                                }
                                LCoreOperator::If => {
                                    scopes.new_scope();
                                    let stack = IfFrame {
                                        conseq: args[1].clone(),
                                        alt: args[2].clone(),
                                    };

                                    queue.push(StackFrame::new(stack, interruptibility));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
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
                                            .map(|a| {
                                                StackFrame::new_lvalue(a.clone(), interruptibility)
                                            })
                                            .collect(),
                                    );
                                }
                                LCoreOperator::Do => {
                                    scopes.new_scope();
                                    let stack = DoFrame {
                                        results: vec![],
                                        rest: args[1..].to_vec(),
                                    };

                                    queue.push(StackFrame::new(stack, interruptibility));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LCoreOperator::Async => {
                                    let result =
                                        async_eval(args[0].clone(), scopes.get_last().clone());

                                    results.push(result.into());
                                    debug.print_last_result(&results);
                                }
                                LCoreOperator::Await => {
                                    //println!("awaiting on async evaluation");
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Await,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
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
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LCoreOperator::Expand => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Expand,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LCoreOperator::Parse => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Parse,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }

                                LCoreOperator::Interrupt => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Interrupt,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LCoreOperator::Interruptible => {
                                    unreachable!();
                                    //queue.push(StackFrame::interruptible(args[0].clone()))
                                }
                                LCoreOperator::Uninterruptible => {
                                    unreachable!();
                                    //queue.push(StackFrame::uninterruptible(args[0].clone()))
                                }
                                /*LCoreOperator::QuasiInterruptible => {
                                    queue.push(StackFrame::quasiinterruptible(args[0].clone()))
                                }*/
                                LCoreOperator::Race => {
                                    let mut handler_1 =
                                        async_eval(args[0].clone(), scopes.get_last().clone());
                                    let mut handler_2 =
                                        async_eval(args[1].clone(), scopes.get_last().clone());

                                    let (tx, mut rx) = new_interruption_handler();

                                    let future: FutureResult = Box::pin(async move {
                                        let future_1 = handler_1.get_future();
                                        let future_2 = handler_2.get_future();
                                        tokio::select! {
                                            r1 = future_1 => {
                                                let r2 = handler_2.interrupt().await;
                                                match (r1, r2) {
                                                    (Ok(l1), Ok(l2)) => {
                                                        Ok(list![l1, l2])
                                                    }
                                                    (Err(e1), Ok(l2)) => {
                                                        Err(e1.chain(format!("Error on race because first expression returned a runtime error.\nResult of first expression: {}", l2)))
                                                    }
                                                    (Ok(l1), Err(e2)) => {
                                                        Err(e2.chain(format!("Error on race because interruption of second expression returned a runtime error.\nResult of second expression: {}", l1)))
                                                    }
                                                    (Err(e1), Err(e2)) => {
                                                        Err(e2.chain(e1).chain("Error on race because both expressions returned runtime errors."))
                                                    }
                                                }
                                            }
                                            r2 = future_2 =>  {
                                                let r1 = handler_1.interrupt().await;
                                                match (r1, r2) {
                                                    (Ok(l1), Ok(l2)) => {
                                                        Ok(list![l1, l2])
                                                    }
                                                    (Err(e1), Ok(l2)) => {
                                                        Err(e1.chain(format!("Error on race because interruption of first expression returned a runtime error.\nResult of first expression: {}", l2)))
                                                    }
                                                    (Ok(l1), Err(e2)) => {
                                                        Err(e2.chain(format!("Error on race because second expression returned a runtime error.\nResult of second expression: {}", l1)))
                                                    }
                                                    (Err(e1), Err(e2)) => {
                                                        Err(e1.chain(e2).chain("Error on race because both expressions returned runtime errors."))
                                                    }
                                                }
                                            }
                                            _ = rx.recv() => {
                                                let r1 = handler_1.interrupt().await;
                                                let r2 = handler_2.interrupt().await;
                                                 match (r1, r2) {
                                                    (Ok(l1), Ok(l2)) => {
                                                        Ok(LValue::Err(Arc::new(list![INTERRUPTED.into(),l1, l2])))                                                    }
                                                        (Err(e1), Ok(l2)) => {
                                                        Err(e1.chain(format!("Error on race because interruption of first expression returned a runtime error.\nResult of first expression: {}", l2)))
                                                    }
                                                    (Ok(l1), Err(e2)) => {
                                                        Err(e2.chain(format!("Error on race because interruption of second expression returned a runtime error.\nResult of second expression: {}", l1)))
                                                    }
                                                    (Err(e1), Err(e2)) => {
                                                        Err(e1.chain(e2).chain("Error on race because both expressions interruptions returned runtime errors."))
                                                    }
                                                }

                                            }
                                        }
                                    })
                                        as FutureResult;
                                    let future: LFuture = future.shared();

                                    results.push(LAsyncHandler::new(future, tx).into());
                                    debug.print_last_result(&results);
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
                                    .map(|a| StackFrame::new_lvalue(a.clone(), interruptibility))
                                    .collect(),
                            );
                        }
                    }
                    _ => {
                        results.push(lv.clone());
                        debug.print_last_result(&results);
                    }
                }
            }
            StackKind::Procedure(ref pro) => {
                let exps: Vec<LValue> = results.pop_n(pro.n);
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        queue.push(StackFrame::new(CoreOperatorFrame::Lambda, interruptibility));
                        queue.push(StackFrame::new_lvalue(
                            l.get_body().clone(),
                            interruptibility,
                        ));
                        let temp_env = match l.get_new_env(scopes.get_last().clone(), args) {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        scopes.new_defined_scope(temp_env);
                    }
                    LValue::Fn(fun) => {
                        let r_lvalue = match fun.call(scopes.get_last(), args) {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.print_last_result(&results);
                        scopes.revert_scope();
                    }
                    LValue::AsyncFn(fun) => {
                        let r_lvalue = match fun.call(scopes.get_last(), args).await {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.print_last_result(&results);
                        scopes.revert_scope();
                    }
                    lv => {
                        let e = wrong_type!("eval", lv, KindLValue::Fn);
                        expression_error = exps.into();
                        break Err(e);
                    }
                }
            }
            StackKind::CoreOperator(cos) => match cos {
                CoreOperatorFrame::Define(d) => {
                    let env = scopes.get_last_mut();
                    let value = results.pop().unwrap();
                    env.insert(d.symbol.as_ref(), value);
                    results.push(LValue::Nil);
                    debug.print_last_result(&results);
                }
                CoreOperatorFrame::If(i) => {
                    let result = results.pop().unwrap();

                    match result {
                        LValue::True => {
                            queue.push(StackFrame::new_lvalue(i.conseq.clone(), interruptibility))
                        }
                        LValue::Nil => {
                            queue.push(StackFrame::new_lvalue(i.alt.clone(), interruptibility))
                        }
                        lv => {
                            let e = wrong_type!("eval", &lv, KindLValue::Bool);
                            expression_error = list![LCoreOperator::If.into(), lv, i.conseq, i.alt];
                            break Err(e.chain("if condition must return a boolean."));
                        }
                    };
                    scopes.revert_scope()
                }
                CoreOperatorFrame::Do(mut df) => {
                    if df.rest.is_empty() {
                        scopes.revert_scope();
                    } else {
                        let result = results.pop().unwrap();
                        df.results.push(result.clone());
                        if matches!(result, LValue::Err(_)) {
                            results.push(result);
                            scopes.revert_scope();
                        } else {
                            let next = df.rest.remove(0);
                            queue.push(StackFrame::new(df, interruptibility));
                            queue.push(StackFrame::new_lvalue(next, interruptibility));
                        }
                    }
                }
                CoreOperatorFrame::Begin(b) => {
                    let mut r = results.pop_n(b.n);
                    results.push(r.pop().unwrap());
                    debug.print_last_result(&results);
                    scopes.revert_scope();
                }
                CoreOperatorFrame::Lambda => {
                    scopes.revert_scope();
                    scopes.revert_scope();
                }
                CoreOperatorFrame::Await => {
                    let result = results.pop().unwrap();
                    if let LValue::Handler(ref h) = result {
                        let f = h.get_future();

                        let r: LResult = if let Some(int) = &mut int {
                            tokio::select! {
                                r = f => {
                                    r
                                }
                                _ = int.recv() => {
                                    Ok(LValue::Err(Arc::new("interrupted".into())))
                                }
                            }
                        } else {
                            f.await
                        };

                        match r {
                            Err(e) => {
                                expression_error = list![LCoreOperator::Await.into(), result];
                                break Err(e);
                            }
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        expression_error = list![LCoreOperator::Await.into(), result.clone()];
                        break Err(wrong_type!(EVAL, &result, KindLValue::Handler)
                            .chain("Await argument must be a LValue::Handler."));
                    };
                }
                CoreOperatorFrame::Interrupt => {
                    let mut result = results.pop().unwrap();
                    if let LValue::Handler(ref mut h) = result {
                        match h.interrupt().await {
                            Err(e) => {
                                expression_error = list![LCoreOperator::Await.into(), result];
                                break Err(e);
                            }
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        let e = wrong_type!(EVAL, &result, KindLValue::Handler);
                        expression_error = list![LCoreOperator::Await.into(), result];
                        break Err(e.chain("Interrupt argument must be a LValue::Handler."));
                    };
                }
                CoreOperatorFrame::Eval => {
                    queue.push(StackFrame::new_lvalue(
                        results.pop().unwrap(),
                        interruptibility,
                    ));
                }
                CoreOperatorFrame::Expand => {
                    let result = results.pop().unwrap();
                    results.push(expand(&result, true, scopes.get_last_mut()).await?);
                    debug.print_last_result(&results);
                }
                CoreOperatorFrame::Parse => {
                    let result = results.pop().unwrap();
                    if let LValue::String(s) = result {
                        results.push(parse(s.as_str(), scopes.get_last_mut()).await?);
                        debug.print_last_result(&results);
                    } else {
                        let e = wrong_type!("eval", &result, KindLValue::String);
                        expression_error = list![LCoreOperator::Parse.into(), result];
                        break Err(e.chain("Parse argument must be a string."));
                    };
                }
            },
        }
    };

    match result {
        Ok(_) => result,
        Err(e) => unstack(expression_error, e, results, queue),
    }
}

pub fn unstack(
    current: LValue,
    e: LRuntimeError,
    mut results: Results,
    mut queue: EvalStack,
) -> LResult {
    results.push(string!(format!("[{} => {}]", current, e.get_message())));

    //loop to unstack and print where the error occured

    while let Some(s) = queue.pop() {
        let result = s.unstack(&mut results);
        results.push(result);
    }
    Err(e.chain(format!("Scheme :\n{}", results.pop().unwrap().format(0)).as_str()))
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
