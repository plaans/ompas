use crate::conversion::flow::p_eval::r#struct::PConfig;
use async_recursion::async_recursion;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::exec::resource::{ACQUIRE, MAX_Q, QUANTITY};
use sompas_core::*;
use sompas_language::primitives::DO;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LambdaArgs;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_n_args, wrong_type};
use std::fmt::Write;

pub const TRANSFORM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub async fn pre_processing(lv: &LValue, env: &LEnv) -> LResult {
    let env = &mut env.clone();
    let mut pc = PConfig::default();
    pc.avoid.insert(EXEC_TASK.to_string());
    let lv = lambda_expansion(&lv, env, &pc.avoid).await?;
    let lv = acquire_expansion(&lv, env).await?;
    let lv = do_expansion(&lv, env).await?;
    //println!("{}", lv.format(0));

    Ok(lv)
}

/*pub fn pre_eval(lv: &LValue, _context: &ConversionContext) -> LResult {
    //let mut env = context.env.clone();
    //let plv = eval_static(lv, &mut env)?;
    //Ok(plv.get_lvalue().clone());
    Ok(lv.clone())
}*/

#[async_recursion]
pub async fn lambda_expansion(lv: &LValue, env: &LEnv, avoid: &im::HashSet<String>) -> LResult {
    let mut lv = match transform_lambda_expression(lv, &mut env.clone(), avoid).await {
        Ok(lv) => lv,
        Err(_) => lv.clone(),
    };

    if let LValue::List(list) = &lv {
        let mut result = vec![];
        for lv in list.iter() {
            result.push(lambda_expansion(lv, env, avoid).await?)
        }

        lv = result.into()
    }

    Ok(lv)
}

pub async fn transform_lambda_expression(
    lv: &LValue,
    env: &mut LEnv,
    avoid: &im::HashSet<String>,
) -> LResult {
    //println!("in transform lambda");

    if let LValue::List(list) = lv {
        if list.is_empty() {
            return Err(LRuntimeError::wrong_number_of_args(
                TRANSFORM_LAMBDA_EXPRESSION,
                list.as_slice(),
                1..std::usize::MAX,
            ));
        }

        let arg = list[0].clone();

        if !avoid.contains(&arg.to_string()) {
            let lambda = eval(&expand(&arg, true, env).await?, env, None)
                .await
                .expect("Error in thread evaluating lambda");
            //println!("evaluating is a success");
            if let LValue::Lambda(l) = lambda {
                let mut lisp = vec![LPrimitive::Begin.into()];

                let args = &list[1..];

                let params = l.get_params();
                let body = l.get_body();

                match params {
                    LambdaArgs::Sym(param) => {
                        let arg = if args.len() == 1 {
                            match &args[0] {
                                LValue::Nil => LValue::Nil,
                                _ => vec![args[0].clone()].into(),
                            }
                        } else {
                            args.into()
                        };
                        lisp.push(list![LPrimitive::Define.into(), param.into(), arg]);
                    }
                    LambdaArgs::List(params) => {
                        if params.len() != args.len() {
                            return Err(wrong_n_args!(
                                TRANSFORM_LAMBDA_EXPRESSION,
                                args,
                                params.len()
                            )
                            .chain("in lambda")
                            .chain(TRANSFORM_LAMBDA_EXPRESSION));
                        }
                        for (param, arg) in params.iter().zip(args) {
                            lisp.push(list![LPrimitive::Define.into(), param.into(), arg.clone()]);
                        }
                    }
                    LambdaArgs::Nil => {
                        if !args.is_empty() {
                            return Err(lruntimeerror!(
                                TRANSFORM_LAMBDA_EXPRESSION,
                                "Lambda was expecting no args.".to_string()
                            ));
                        }
                    }
                };
                lisp.push(body.clone());

                Ok(lisp.into())
            } else {
                Err(wrong_type!(
                    TRANSFORM_LAMBDA_EXPRESSION,
                    &list[0],
                    KindLValue::Lambda
                ))
            }
        } else {
            Err(wrong_type!(
                TRANSFORM_LAMBDA_EXPRESSION,
                &list[0],
                KindLValue::Lambda
            ))
        }
    } else {
        Err(wrong_type!(
            TRANSFORM_LAMBDA_EXPRESSION,
            lv,
            KindLValue::List
        ))
    }
}

#[async_recursion]
pub async fn acquire_expansion(lv: &LValue, env: &LEnv) -> LResult {
    if let LValue::List(list) = &lv {
        let mut result: Vec<LValue> = vec![];
        for lv in list.iter() {
            result.push(acquire_expansion(lv, env).await?);
        }

        if result[0].to_string().as_str() == ACQUIRE {
            let mut lisp = "(begin ".to_string();
            match result.len() {
                2 => {
                    write!(lisp, "(define __r__ {})", result[1]).unwrap();
                    write!(lisp, "(define __q__ (read-state {MAX_Q} __r__))").unwrap();
                }
                3 => {
                    write!(lisp, "(define __r__ {})", result[1]).unwrap();
                    write!(lisp, "(define __q__ {}", result[2]).unwrap();
                }
                _ => unreachable!(),
            }

            write!(lisp, "(wait-for (<= __q__ (read-state {QUANTITY} __r__)))").unwrap();
            write!(
                lisp,
                "(assert {QUANTITY} __r__ (+ (read-state {QUANTITY} __r__) __q__))"
            )
            .unwrap();
            write!(lisp, "(ressource-handle (assert {QUANTITY} __r__ (- (read-state {QUANTITY} __r__) __q__))))").unwrap();
            let mut c_env = env.clone();

            parse(&lisp, &mut c_env).await
        } else {
            Ok(result.into())
        }
    } else {
        Ok(lv.clone())
    }
}

#[async_recursion]
pub async fn do_expansion(lv: &LValue, env: &LEnv) -> LResult {
    if let LValue::List(list) = &lv {
        let mut result: Vec<LValue> = vec![];
        for lv in list.iter() {
            result.push(do_expansion(lv, env).await?);
        }

        if result[0].to_string().as_str() == DO {
            let mut lisp = "".to_string();

            let n = result.len() - 1;
            for (i, e) in result[1..].iter().enumerate() {
                if i == n - 1 {
                    write!(lisp, "{e}").unwrap();
                } else {
                    write!(
                        lisp,
                        "(begin\
                    (define __r__ {e})\
                    (if (err? __r__) __r__ "
                    )
                    .unwrap();
                }
            }
            write!(lisp, "{}", ")".repeat((n - 1) * 2)).unwrap();

            let mut c_env = env.clone();

            parse(&lisp, &mut c_env).await
        } else {
            Ok(result.into())
        }
    } else {
        Ok(lv.clone())
    }
}
