extern crate core;

use crate::modules::ModStd;
use crate::structs::{
    BeginFrame, CoreOperatorFrame, DefineFrame, DoFrame, EvalStack, IfFrame, Interruptibility,
    LDebug, ProcedureFrame, Results, ScopeCollection, StackFrame, StackKind, Unstack,
};
use anyhow::anyhow;
use aries_planning::parsing::sexpr::SExpr;
use async_recursion::async_recursion;
use futures::FutureExt;
use sompas_language::kind::*;
use sompas_language::list::CONS;
use sompas_language::primitives::*;
use sompas_language::{primitives, FALSE};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::{new_interruption_handler, InterruptionReceiver};
use sompas_structs::lvalue::LValue;
use sompas_structs::{interrupted, list, string, symbol, wrong_n_args, wrong_type};
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fmt::Write;
use std::ops::Deref;
use std::sync::Arc;

pub mod modules;
pub mod structs;
pub mod test_utils;

pub const SOMPAS_DEBUG: &str = "SOMPAS_DEBUG";

pub async fn get_root_env() -> LEnv {
    let mut env = LEnv::default();
    env.import_module(ModStd::default(), ImportType::WithoutPrefix);
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
            let str = atom.to_string();
            let canonical = atom.canonical_str();
            match str.parse::<i64>() {
                Ok(int) => LValue::Number(LNumber::Int(int)),
                Err(_) => match str.parse::<f64>() {
                    //Test if its a float
                    Ok(float) => LValue::Number(LNumber::Float(float)),
                    Err(_) => match canonical {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            LValue::True
                        }
                        FALSE | NIL => {
                            //println!("atom is boolean false");
                            LValue::Nil
                        }
                        _ => {
                            if str.starts_with('\"') && str.ends_with('\"') {
                                //println!("new string: {}", s);
                                string!(str[1..str.len() - 1].to_string())
                            } else {
                                symbol!(canonical.to_string())
                            }
                        }
                    },
                },
            }
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
            if let Ok(co) = LPrimitive::try_from(&list[0]) {
                match co {
                    LPrimitive::Define | LPrimitive::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(LRuntimeError::wrong_number_of_args(
                                EXPAND,
                                list.as_slice(),
                                3..usize::MAX,
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
                                if def == LPrimitive::DefMacro {
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
                                return Ok(vec![LPrimitive::Define.into(), v.clone(), exp].into());
                            }
                            _ => return Err(wrong_type!("expand", x, KindLValue::Symbol)),
                        }
                    }
                    LPrimitive::DefLambda => {
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
                            let mut vec = vec![LPrimitive::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(Arc::new(vec))
                        };
                        return Ok(vec![
                            LPrimitive::DefLambda.into(),
                            vars.clone(),
                            expand(&exp, top_level, env).await?,
                        ]
                        .into());
                    }
                    LPrimitive::If => {
                        let mut list = list.deref().clone();
                        if list.len() == 3 {
                            list.push(LValue::Nil);
                        }
                        if list.len() != 4 {
                            return Err(wrong_n_args!("expand", list.as_slice(), 4));
                        }
                        //return map(expand, x)
                        let mut expanded_list = vec![LPrimitive::If.into()];
                        for x in &list[1..] {
                            expanded_list.push(expand(x, false, env).await?)
                        }
                        return Ok(expanded_list.into());
                    }
                    LPrimitive::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(wrong_n_args!("expand", list.as_slice(), 2));
                        }
                        return Ok(vec![LPrimitive::Quote.into(), list[1].clone()].into());
                    }
                    LPrimitive::Begin | LPrimitive::Do => {
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
                    LPrimitive::QuasiQuote => {
                        return if list.len() != 2 {
                            return Err(wrong_n_args!("expand", list, 2));
                        } else {
                            expand(&expand_quasi_quote(&list[1], env)?, top_level, env).await
                        };
                    }
                    LPrimitive::UnQuote => {
                        return Err(anyhow!("unquote must be inside a quasiquote expression").into())
                    }
                    LPrimitive::Async => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list.as_slice(), 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Async.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Await => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list.as_slice(), 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Await.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Eval => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Eval.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Enr => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Enr.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Parse => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Parse.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Expand => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Expand.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Interrupt => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Interrupt.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Interruptible => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2))
                        } else {
                            let mut expanded = vec![LPrimitive::Interruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Uninterruptible => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2)
                                .chain(format!("{} must have one arg", UNINTERRUPTIBLE)))
                        } else {
                            let mut expanded = vec![LPrimitive::Uninterruptible.into()];
                            expanded.push(expand(&list[1], top_level, env).await?);
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Race => {
                        return if list.len() != 3 {
                            Err(wrong_n_args!("expand", list, 3)
                                .chain(format!("{} must have 2 args.", RACE)))
                        } else {
                            let mut expanded = vec![LPrimitive::Race.into()];
                            for e in &list[1..] {
                                expanded.push(expand(e, top_level, env).await?);
                            }
                            Ok(expanded.into())
                        }
                    }
                    LPrimitive::Err => {
                        return if list.len() != 2 {
                            Err(wrong_n_args!("expand", list, 2)
                                .chain(format!("{} must have one arg", primitives::ERR)))
                        } else {
                            let mut expanded = vec![LPrimitive::Err.into()];
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

                        let expanded = expand(&eval(lv, env, None).await?, top_level, env).await?;
                        //if get_debug() {
                        env.log
                            .trace(format!("In expand: macro expanded: {}", expanded));
                        //}
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
                    if let Ok(co) = LPrimitive::try_from(s.as_str()) {
                        if co == LPrimitive::UnQuote {
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
        _ => Ok(vec![LPrimitive::Quote.into(), x.clone()].into()),
    }
}

#[inline]
pub fn async_eval(lv: LValue, mut env: LEnv) -> LAsyncHandle {
    let (tx, rx) = new_interruption_handler();

    let future: FutureResult =
        Box::pin(async move { eval(&lv, &mut env, Some(rx)).await }) as FutureResult;
    let future: LFuture = future.shared();

    tokio::spawn(future.clone());

    LAsyncHandle::new(future, tx)
}

/// Evaluate a LValue
/// Main function of the Scheme Interpreter
#[async_recursion]
pub async fn eval(
    lv: &LValue,
    root_env: &mut LEnv,
    mut int: Option<InterruptionReceiver>,
) -> LResult {
    let log = root_env.log.clone();
    let mut debug: LDebug = Default::default();
    debug.log = log.clone();
    //debug.push(Unininterruptible, lv);

    let mut interrupted = false;
    let error = interrupted!();
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
            if interrupted {
                log.trace(format!("interrupted! last result!: {:?}", results.last()));
            }
        }

        let interruptibility = current.interruptibily;

        if interrupted && interruptibility == Interruptibility::Interruptible {
            match current.kind {
                StackKind::NonEvaluated(_) => {
                    results.push(error.clone());
                }
                StackKind::Procedure(p) => {
                    results.pop_n(p.n);
                    scopes.revert_scope();
                    results.push(error.clone());
                    debug.log_last_result(&results);
                }
                StackKind::CoreOperator(co) => {
                    match co {
                        CoreOperatorFrame::If(_) => {
                            results.pop();
                            results.push(error.clone());
                            scopes.revert_scope();
                        }
                        CoreOperatorFrame::Begin(b) => {
                            let mut r = results.pop_n(b.n);
                            scopes.revert_scope();
                            results.push(r.pop().unwrap());
                        }
                        CoreOperatorFrame::Do(_) => {
                            scopes.revert_scope();
                            //results.pop();
                        }
                        CoreOperatorFrame::Define(_) => {
                            scopes.revert_scope();
                            results.pop();
                            results.push(error.clone());
                        }
                        CoreOperatorFrame::Lambda => {
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
                        CoreOperatorFrame::Enr => {
                            results.pop();
                            results.push(error.clone());
                        }
                        CoreOperatorFrame::EvalEnd => {
                            scopes.revert_scope();
                            results.pop();
                            results.push(error.clone());
                        }
                        CoreOperatorFrame::EnrEnd => {
                            scopes.revert_scope();
                            results.pop();
                            results.push(error.clone());
                        }
                        CoreOperatorFrame::IfEnd => {
                            scopes.revert_scope();
                            results.pop();
                            results.push(error.clone())
                        }
                    }
                    debug.log_last_result(&results);
                }
            }
            continue;
        }

        match current.kind {
            StackKind::NonEvaluated(ref lv) => {
                debug.push(current.interruptibily, lv.to_string());
                match &lv {
                    LValue::Symbol(s) => {
                        let scope = scopes.get_last();
                        let str = s.as_str();
                        let result = match scope.get_symbol(str) {
                            Some(lv) => lv,
                            None => s.into(),
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
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::If => {
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
                                LPrimitive::Quote => {
                                    results.push(args[0].clone());
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::Err => {
                                    results.push(LValue::Err(args[0].clone().into_ref()));
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
                                LPrimitive::Do => {
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
                                LPrimitive::Async => {
                                    let result =
                                        async_eval(args[0].clone(), scopes.get_last().clone());

                                    results.push(result.into());
                                    debug.log_last_result(&results);
                                }
                                LPrimitive::Await => {
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
                                LPrimitive::Eval => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Eval,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LPrimitive::Enr => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Enr,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LPrimitive::Expand => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Expand,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LPrimitive::Parse => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Parse,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }

                                LPrimitive::Interrupt => {
                                    queue.push(StackFrame::new(
                                        CoreOperatorFrame::Interrupt,
                                        interruptibility,
                                    ));
                                    queue.push(StackFrame::new_lvalue(
                                        args[0].clone(),
                                        interruptibility,
                                    ));
                                }
                                LPrimitive::Interruptible => {
                                    unreachable!();
                                }
                                LPrimitive::Uninterruptible => {
                                    unreachable!();
                                }
                                LPrimitive::Race => {
                                    let handler_1 =
                                        async_eval(args[0].clone(), scopes.get_last().clone());
                                    let handler_2 =
                                        async_eval(args[1].clone(), scopes.get_last().clone());

                                    let (tx, mut rx) = new_interruption_handler();

                                    let future: FutureResult = Box::pin(async move {
                                        let future_1 = handler_1.get_future();
                                        let future_2 = handler_2.get_future();
                                        tokio::select! {
                                            r1 = future_1 => {
                                                //handler_2.interrupt();
                                                r1
                                                /*match (r1, r2) {
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
                                                }*/
                                            }
                                            r2 = future_2 =>  {
                                                //handler_1.interrupt();
                                                r2
                                                /*match (r1, r2) {
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
                                                }*/
                                            }
                                            _ = rx.recv() => {
                                                //handler_1.interrupt();
                                                //handler_2.interrupt();

                                                Ok(interrupted!())

                                                /*let r1 = r1.await;
                                                let r2 = r2.await;

                                                 match (r1, r2) {
                                                    (Ok(l1), Ok(l2)) => {
                                                        Ok(list!(l1,l2))
                                                    }
                                                    (Err(e1), Ok(l2)) => {
                                                        Err(e1.chain(format!("Error on race because interruption of first expression returned a runtime error.\nResult of first expression: {}", l2)))
                                                    }
                                                    (Ok(l1), Err(e2)) => {
                                                        Err(e2.chain(format!("Error on race because interruption of second expression returned a runtime error.\nResult of second expression: {}", l1)))
                                                    }
                                                    (Err(e1), Err(e2)) => {
                                                        Err(e1.chain(e2).chain("Error on race because both expressions interruptions returned runtime errors."))
                                                    }
                                                }*/

                                            }
                                        }
                                    })
                                        as FutureResult;
                                    let future: LFuture = future.shared();

                                    results.push(LAsyncHandle::new(future, tx).into());
                                    debug.log_last_result(&results);
                                }
                            }
                        } else if scopes
                            .get_last()
                            .get_macro(proc.to_string().as_str())
                            .is_some()
                        {
                            queue.push(StackFrame::new(CoreOperatorFrame::Eval, interruptibility));
                            queue
                                .push(StackFrame::new(CoreOperatorFrame::Expand, interruptibility));
                            debug.push(
                                current.interruptibily,
                                format!("({} {})", LPrimitive::Expand, lv),
                            );
                            results.push(lv.clone());
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
                        debug.log_last_result(&results);
                    }
                }
            }
            StackKind::Procedure(ref pro) => {
                let exps: Vec<LValue> = results.pop_n(pro.n);
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        let temp_env = match l.get_new_env(scopes.get_last().clone(), args) {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        queue.push(StackFrame::new(CoreOperatorFrame::Lambda, interruptibility));
                        queue.push(StackFrame::new_lvalue(
                            l.get_body().clone(),
                            interruptibility,
                        ));
                        scopes.revert_scope();
                        scopes.new_defined_scope(temp_env);
                    }
                    LValue::Fn(fun) => {
                        scopes.revert_scope();
                        let r_lvalue = match fun.call(scopes.get_last(), args) {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.log_last_result(&results);
                    }
                    LValue::MutFn(fun) => {
                        scopes.revert_scope();
                        let r_lvalue = match fun.call(scopes.get_last_mut(), args) {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.log_last_result(&results);
                    }
                    LValue::AsyncFn(fun) => {
                        scopes.revert_scope();
                        let r_lvalue = match fun.call(scopes.get_last(), args).await {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.log_last_result(&results);
                    }
                    LValue::AsyncMutFn(fun) => {
                        scopes.revert_scope();
                        let r_lvalue = match fun.call(scopes.get_last_mut(), args).await {
                            Ok(e) => e,
                            Err(e) => {
                                expression_error = exps.into();
                                break Err(e);
                            }
                        };
                        results.push(r_lvalue);
                        debug.log_last_result(&results);
                    }
                    lv => {
                        scopes.revert_scope();
                        let e = wrong_type!("eval", lv, KindLValue::Fn);
                        expression_error = exps.into();
                        break Err(e);
                    }
                }
            }
            StackKind::CoreOperator(cos) => match cos {
                CoreOperatorFrame::Define(d) => {
                    scopes.revert_scope();
                    let env = scopes.get_last_mut();
                    let value = results.pop().unwrap();
                    env.insert(d.symbol.as_ref(), value);
                    results.push(LValue::Nil);
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::If(i) => {
                    let result = results.pop().unwrap();

                    match result {
                        LValue::True => {
                            queue.push(StackFrame::new(CoreOperatorFrame::IfEnd, interruptibility));
                            queue.push(StackFrame::new_lvalue(i.conseq.clone(), interruptibility))
                        }
                        LValue::Nil => {
                            queue.push(StackFrame::new(CoreOperatorFrame::IfEnd, interruptibility));
                            queue.push(StackFrame::new_lvalue(i.alt.clone(), interruptibility))
                        }
                        lv => {
                            let e = wrong_type!("eval", &lv, KindLValue::Bool);
                            expression_error = list![LPrimitive::If.into(), lv, i.conseq, i.alt];
                            break Err(e.chain("if condition must return a boolean."));
                        }
                    };
                    scopes.revert_scope();
                    scopes.new_scope();
                }
                CoreOperatorFrame::Do(mut df) => {
                    if df.rest.is_empty() {
                        debug.log_last_result(&results);
                        scopes.revert_scope();
                    } else {
                        let result = results.pop().unwrap();
                        df.results.push(result.clone());
                        if matches!(result, LValue::Err(_)) {
                            results.push(result);
                            debug.log_last_result(&results);
                            scopes.revert_scope();
                        } else {
                            let next = df.rest.remove(0);
                            queue.push(StackFrame::new(df, interruptibility));
                            queue.push(StackFrame::new_lvalue(next, interruptibility));
                        }
                    }
                }
                CoreOperatorFrame::Begin(b) => {
                    scopes.revert_scope();
                    let mut r = results.pop_n(b.n);
                    results.push(r.pop().unwrap());
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::Lambda => {
                    scopes.revert_scope();
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::Await => {
                    let result = results.pop().unwrap();
                    if let LValue::Handle(ref h) = result {
                        let f = h.get_future();

                        let r: LResult = if interruptibility == Interruptibility::Interruptible
                            && int.is_some()
                        {
                            let int = int.as_mut().unwrap();
                            tokio::select! {
                                r = f => {
                                    r
                                }
                                _ = int.recv() => {
                                    //println!("await interrupted");
                                    Ok(interrupted!())
                                }
                            }
                        } else {
                            f.await
                        };

                        match r {
                            Err(e) => {
                                expression_error = list![LPrimitive::Await.into(), result];
                                break Err(e);
                            }
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        expression_error = list![LPrimitive::Await.into(), result.clone()];
                        break Err(wrong_type!(EVAL, &result, KindLValue::Handler)
                            .chain("Await argument must be a LValue::Handler."));
                    };
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::Interrupt => {
                    let mut result = results.pop().unwrap();
                    if let LValue::Handle(ref mut h) = result {
                        match h.interrupt().await {
                            Err(e) => {
                                expression_error = list![LPrimitive::Await.into(), result];
                                break Err(e);
                            }
                            Ok(lv) => results.push(lv),
                        }
                    } else {
                        let e = wrong_type!(EVAL, &result, KindLValue::Handler);
                        expression_error = list![LPrimitive::Await.into(), result];
                        break Err(e.chain("Interrupt argument must be a LValue::Handler."));
                    };
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::Eval => {
                    scopes.new_scope();
                    queue.push(StackFrame::new(
                        CoreOperatorFrame::EvalEnd,
                        interruptibility,
                    ));
                    let r = results.pop().unwrap();
                    //debug.push(interruptibility, list![LPrimitive::Eval.into(), r.clone()]);
                    queue.push(StackFrame::new_lvalue(r, interruptibility));
                }
                CoreOperatorFrame::Expand => {
                    let result = results.pop().unwrap();
                    scopes.new_scope();
                    results.push(expand(&result, true, scopes.get_last_mut()).await?);
                    scopes.revert_scope();
                    //debug.push(Unininterruptible, list!(LPrimitive::Expand.into(), result));
                    debug.log_last_result(&results);
                }
                CoreOperatorFrame::Parse => {
                    let result = results.pop().unwrap();
                    if let LValue::String(s) = &result {
                        scopes.new_scope();
                        results.push(parse(s.as_str(), scopes.get_last_mut()).await?);
                        scopes.revert_scope();
                        //debug.push(Unininterruptible, list!(LPrimitive::Parse.into(), result));
                        debug.log_last_result(&results);
                    } else {
                        let e = wrong_type!("eval", &result, KindLValue::String);
                        expression_error = list![LPrimitive::Parse.into(), result];
                        break Err(e.chain("Parse argument must be a string."));
                    };
                }
                CoreOperatorFrame::Enr => {
                    let expr: LValue = results.pop().unwrap();
                    let expr = match expr {
                        LValue::List(list) => {
                            let mut new_expr = vec![list[0].clone()];
                            for e in &list.as_slice()[1..] {
                                new_expr.push(list!(LPrimitive::Quote.into(), e.clone()))
                            }
                            new_expr.into()
                        }
                        expr => expr,
                    };
                    scopes.new_scope();
                    queue.push(StackFrame::new(CoreOperatorFrame::EnrEnd, interruptibility));
                    queue.push(StackFrame::new_lvalue(expr, interruptibility));
                }
                CoreOperatorFrame::EvalEnd
                | CoreOperatorFrame::EnrEnd
                | CoreOperatorFrame::IfEnd => {
                    scopes.revert_scope();
                    debug.log_last_result(&results);
                }
            },
        }
    };

    match result {
        Ok(_) => {
            assert_eq!(results.len(), 0);
            assert_eq!(debug.len(), 0);
            assert_eq!(scopes.len(), 1);
            result
        }
        Err(e) => unstack(expression_error, e, results, queue),
    }
}

fn unstack(
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
