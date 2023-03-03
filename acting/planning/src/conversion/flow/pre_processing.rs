use crate::conversion::flow::p_eval::r#struct::PLEnv;
use async_recursion::async_recursion;
use sompas_core::*;
use sompas_language::error::IS_ERR;
use sompas_language::list::FN_LIST;
use sompas_language::primitives::DO;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::llambda::LambdaArgs;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_n_args, wrong_type};

pub const TRANSFORM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub async fn pre_processing(lv: &LValue, _: &PLEnv) -> LResult {
    //let lv = lambda_expansion(&lv, p_env).await?;
    let lv = do_expansion(lv.clone());
    //println!("{}", lv.format(0));

    Ok(lv)
}

#[async_recursion]
pub async fn lambda_expansion(lv: &LValue, p_env: &PLEnv) -> LResult {
    let mut lv = match transform_lambda_expression(lv, p_env).await {
        Ok(lv) => lv,
        Err(_) => lv.clone(),
    };

    if let LValue::List(list) = &lv {
        let mut result = vec![];
        for lv in list.iter() {
            result.push(lambda_expansion(lv, p_env).await?)
        }

        lv = result.into()
    }

    Ok(lv)
}

pub async fn transform_lambda_expression(lv: &LValue, p_env: &PLEnv) -> LResult {
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

        if !p_env.get_p_config().avoid.contains(&arg.to_string()) {
            let env = &mut p_env.env.clone();
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
                                _ => {
                                    //vec![args[0].clone()].into()
                                    vec![FN_LIST.into(), args[0].clone()].into()
                                }
                            }
                        } else {
                            //let mut vec = vec![];
                            let mut vec = vec![FN_LIST.into()];
                            vec.append(&mut args.to_vec());
                            vec.into()
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

pub fn do_expansion(lv: LValue) -> LValue {
    if let LValue::List(list) = lv {
        let mut result: Vec<LValue> = vec![];
        for lv in list.iter() {
            result.push(do_expansion(lv.clone()));
        }

        if result[0].to_string().as_str() == DO {
            result.remove(0);

            let mut last: Option<LValue> = None;
            let __r__: LValue = "__r__".into();
            let is_err: LValue = IS_ERR.into();
            while let Some(e) = result.pop() {
                last = if let Some(last) = last {
                    Some(list![
                        LPrimitive::Begin.into(),
                        list![LPrimitive::Define.into(), __r__.clone(), e],
                        list![
                            LPrimitive::If.into(),
                            list![is_err.clone(), __r__.clone()],
                            __r__.clone(),
                            last
                        ]
                    ])
                } else {
                    Some(e)
                };
            }
            last.unwrap()
        } else {
            result.into()
        }
    } else {
        lv
    }
}

/*
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
                "(assert {QUANTITY} __r__ (- (read-state {QUANTITY} __r__) __q__))"
            )
            .unwrap();
            write!(lisp, "(ressource-handle (assert {QUANTITY} __r__ (+ (read-state {QUANTITY} __r__) __q__))))").unwrap();
            let mut c_env = env.clone();

            parse(&lisp, &mut c_env).await
        } else {
            Ok(result.into())
        }
    } else {
        Ok(lv.clone())
    }
}*/
