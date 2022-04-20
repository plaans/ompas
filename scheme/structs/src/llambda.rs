use crate::lenv::{LEnv, LEnvSymbols};
use crate::lerror::LRuntimeError;

use crate::lvalue::LValue;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

/// Struct to define a lambda in Scheme.
#[derive(Clone)]
pub struct LLambda {
    params: LambdaArgs,
    body: Arc<LValue>,
    env: LEnvSymbols,
}

impl Debug for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "-lambda {:?} : {:?}\n-env: {:?}",
            self.params, self.body, self.env,
        )
    }
}

impl Display for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "(lambda {} {})", self.params, self.body)
    }
}

impl PartialEq for LLambda {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl LLambda {
    ///Constructs a new lambda, capturing the environment in which it has been created.
    pub fn new(params: LambdaArgs, body: LValue, env: LEnvSymbols) -> Self {
        LLambda {
            params,
            body: Arc::new(body),
            env,
        }
    }

    /// Returns a new env containing the environment of the lambda and the current environment in which the lambda is called.
    pub fn get_new_env(&self, args: &[LValue], mut env: LEnv) -> Result<LEnv, LRuntimeError> {
        env.set_new_top_symbols(self.env.clone());

        match &self.params {
            LambdaArgs::Sym(param) => {
                let arg = if args.len() == 1 {
                    match &args[0] {
                        LValue::Nil => LValue::Nil,
                        _ => vec![args[0].clone()].into(),
                    }
                } else {
                    args.into()
                };
                env.insert(param.to_string(), arg);
            }
            LambdaArgs::List(params) => {
                if params.len() != args.len() {
                    return Err(Anyhow(
                        "get_new_env",
                        format!(
                            "in lambda {}: ",
                            WrongNumberOfArgument(
                                "get_new_env",
                                args.into(),
                                args.len(),
                                params.len()..params.len(),
                            )
                        ),
                    ));
                }
                for (param, arg) in params.iter().zip(args) {
                    env.insert(param.to_string(), arg.clone());
                }
            }
            LambdaArgs::Nil => {
                if !args.is_empty() {
                    return Err(Anyhow(
                        "Lambda.get_env",
                        "Lambda was expecting no args.".to_string(),
                    ));
                }
            }
        };
        Ok(env)
    }

    pub fn get_env_symbols(&self) -> LEnvSymbols {
        self.env.clone()
    }

    /// Returns the body of the lambda
    pub fn get_body(&self) -> &LValue {
        self.body.as_ref()
    }

    pub fn get_params(&self) -> LambdaArgs {
        self.params.clone()
    }
}

impl From<&LLambda> for LValue {
    fn from(l: &LLambda) -> Self {
        LValue::Lambda(l.clone())
    }
}

impl From<LLambda> for LValue {
    fn from(l: LLambda) -> Self {
        (&l).into()
    }
}

/// Kinds of args a LValue::LLambda could receive.
/// The parameters of a lambda function can be defined in two ways:
/// - a unique symbol considered as a list:
/// ``` lisp
/// (lambda args <body>)
/// ;args will be considered as a LValue::List
/// ;This lambda is expected to receive a list of arbitrary length.
/// ```
/// - a list of bound symbols :
/// ``` lisp
/// (lambda (x y z) <body>)
/// ;here each symbol will be bound to a LValue.
/// ;This lambda is expected to receive exactly three arguments.
#[derive(Clone, Debug)]
pub enum LambdaArgs {
    Sym(String),
    List(Vec<String>),
    Nil,
}

impl Display for LambdaArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LambdaArgs::Sym(x) => write!(f, "{}", x),
            LambdaArgs::List(l) => {
                let mut s = String::from("(");
                for (i, e) in l.iter().enumerate() {
                    s.push_str(e);
                    if i != l.len() - 1 {
                        s.push(' ');
                    } else {
                        s.push(')');
                    }
                }
                write!(f, "{}", s)
            }
            LambdaArgs::Nil => write!(f, "nil"),
        }
    }
}

impl From<String> for LambdaArgs {
    fn from(s: String) -> Self {
        LambdaArgs::Sym(s)
    }
}

impl From<Vec<String>> for LambdaArgs {
    fn from(vec_sym: Vec<String>) -> Self {
        LambdaArgs::List(vec_sym)
    }
}
