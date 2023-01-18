use async_recursion::async_recursion;
use ompas_rae_language::exec::refinement::EXEC_TASK;
use ompas_rae_language::exec::resource::ACQUIRE;
use ompas_rae_structs::sym_table::{MAX_Q, QUANTITY};
use sompas_core::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LambdaArgs;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_n_args, wrong_type};
use std::fmt::Write;

pub const TRANSFORM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub async fn pre_processing(lv: &LValue, env: &LEnv) -> LResult {
    let avoid = vec![EXEC_TASK.to_string()];
    let lv = lambda_expansion(lv, env, &avoid).await?;
    let lv = acquire_expansion(&lv, env).await?;

    Ok(lv)
}

/*pub fn pre_eval(lv: &LValue, _context: &ConversionContext) -> LResult {
    //let mut env = context.env.clone();
    //let plv = eval_static(lv, &mut env)?;
    //Ok(plv.get_lvalue().clone());
    Ok(lv.clone())
}*/

#[async_recursion]
pub async fn lambda_expansion(lv: &LValue, env: &LEnv, avoid: &Vec<String>) -> LResult {
    let mut lv = match transform_lambda_expression(lv, env.clone(), avoid).await {
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

pub async fn transform_lambda_expression(lv: &LValue, env: LEnv, avoid: &[String]) -> LResult {
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
        let mut c_env = env.clone();

        if !avoid.contains(&arg.to_string()) {
            let lambda = eval(&expand(&arg, true, &mut c_env).await?, &mut c_env, None)
                .await
                .expect("Error in thread evaluating lambda");
            //println!("evaluating is a success");
            if let LValue::Lambda(l) = lambda {
                let mut lisp = "(begin".to_string();

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
                        lisp.push_str(format!("(define {} {})", param, arg).as_str());
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
                            lisp.push_str(format!("(define {} {})", param, arg).as_str());
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

                lisp.push_str(body.to_string().as_str());
                lisp.push(')');

                let mut c_env = env;

                parse(&lisp, &mut c_env).await
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
