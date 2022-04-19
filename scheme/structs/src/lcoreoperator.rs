use crate::lerror::LError;
use crate::lerror::LError::SpecialError;
use serde::*;
use sompas_language::{
    ASYNC, AWAIT, BEGIN, DEFINE, DEF_MACRO, DO, EVAL, EXPAND, IF, LAMBDA, PARSE, QUASI_QUOTE,
    QUOTE, UNQUOTE,
};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

/// The core operators are Scheme operators that can modify the environment directly,
/// or have special behaviour that could not be done inside classical Scheme functions.
/// - Define: insert a new entry in the environment. A symbol can only be defined once.
/// - DefLambda: creates a new lambda object.
/// - If : basic conditional structure
/// - Quote : operator preventing from evaluating an expression
/// - QuasiQuote: operator preventing from evaluating an expression except unquote expression inside of it.
/// - Unquote : used only inside a QuasiQuote block to still evaluate an expression.
/// - DefMacro: insert a new macro in the environment. A macro must be a lambda.
/// - Set: modify the value of an entry in the environment. Cannot set an undefined symbol
/// - Begin: block that evaluates a list of expression and returns the last result.
/// - Async: Evaluates in an asynchronous task a LValue.
/// - Await: Wait on a pid the result of an async.
/// - Eval: Evaluates an expression.
#[derive(Copy, Clone, PartialOrd, PartialEq, Eq, Debug, Serialize, Deserialize)]
#[serde(untagged, rename_all = "lowercase")]
pub enum LCoreOperator {
    Define,
    DefLambda,
    If,
    Quote,
    QuasiQuote,
    UnQuote,
    DefMacro,
    Begin,
    Do,
    Async,
    Await,
    Parse,
    Expand,
    Eval,
}

impl Display for LCoreOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            LCoreOperator::Define => DEFINE,
            LCoreOperator::DefLambda => LAMBDA,
            LCoreOperator::If => IF,
            LCoreOperator::Quote => QUOTE,
            LCoreOperator::QuasiQuote => QUASI_QUOTE,
            LCoreOperator::UnQuote => UNQUOTE,
            LCoreOperator::DefMacro => DEF_MACRO,
            LCoreOperator::Begin => BEGIN,
            LCoreOperator::Async => ASYNC,
            LCoreOperator::Await => AWAIT,
            LCoreOperator::Eval => EVAL,
            LCoreOperator::Expand => EXPAND,
            LCoreOperator::Parse => PARSE,
            LCoreOperator::Do => DO,
        };

        write!(f, "{}", str)
    }
}

impl TryFrom<&str> for LCoreOperator {
    type Error = LError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            DEFINE => Ok(LCoreOperator::Define),
            LAMBDA => Ok(LCoreOperator::DefLambda),
            IF => Ok(LCoreOperator::If),
            QUOTE => Ok(LCoreOperator::Quote),
            QUASI_QUOTE => Ok(LCoreOperator::QuasiQuote),
            UNQUOTE => Ok(LCoreOperator::UnQuote),
            DEF_MACRO => Ok(LCoreOperator::DefMacro),
            BEGIN => Ok(LCoreOperator::Begin),
            ASYNC => Ok(LCoreOperator::Async),
            AWAIT => Ok(LCoreOperator::Await),
            EVAL => Ok(LCoreOperator::Eval),
            PARSE => Ok(LCoreOperator::Parse),
            EXPAND => Ok(LCoreOperator::Expand),
            DO => Ok(LCoreOperator::Do),
            _ => Err(SpecialError(
                "LCoreOperator::TryFrom<str>",
                "string does not correspond to core operator".to_string(),
            )),
        }
    }
}
