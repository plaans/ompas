use async_recursion::async_recursion;
use sompas_core::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LambdaArgs;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_n_args, wrong_type};
use sompas_utils::blocking_async;

pub const TRANSFORM_LAMBDA_EXPRESSION: &str = "transform-lambda-expression";

pub async fn pre_processing(lv: &LValue, env: &LEnv) -> LResult {
    let lv = lambda_expansion(lv, env).await?;

    Ok(lv)
}

/*pub fn pre_eval(lv: &LValue, _context: &ConversionContext) -> LResult {
    //let mut env = context.env.clone();
    //let plv = eval_static(lv, &mut env)?;
    //Ok(plv.get_lvalue().clone());
    Ok(lv.clone())
}*/

#[async_recursion]
pub async fn lambda_expansion(lv: &LValue, env: &LEnv) -> LResult {
    let mut lv = match transform_lambda_expression(lv, env.clone()).await {
        Ok(lv) => lv,
        Err(_) => lv.clone(),
    };

    if let LValue::List(list) = &lv {
        let mut result = vec![];
        for lv in list.iter() {
            result.push(lambda_expansion(lv, env).await?)
        }

        lv = result.into()
    }

    Ok(lv)
}

pub async fn transform_lambda_expression(lv: &LValue, env: LEnv) -> LResult {
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
                    lisp.push_str(format!("(define {} '{})", param, arg).as_str());
                }
                LambdaArgs::List(params) => {
                    if params.len() != args.len() {
                        return Err(
                            wrong_n_args!(TRANSFORM_LAMBDA_EXPRESSION, args, params.len())
                                .chain("in lambda")
                                .chain(TRANSFORM_LAMBDA_EXPRESSION),
                        );
                    }
                    for (param, arg) in params.iter().zip(args) {
                        lisp.push_str(format!("(define {} '{})", param, arg).as_str());
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

            blocking_async!(parse(&lisp, &mut c_env).await).expect("error in thread parsing string")
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
