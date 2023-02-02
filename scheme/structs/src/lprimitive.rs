use crate::lruntimeerror::LRuntimeError;
use crate::lvalue::{LValue, RefLValue};
use serde::*;
use sompas_language::primitives::*;
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
#[derive(Hash, Copy, Clone, PartialOrd, PartialEq, Eq, Debug, Serialize, Deserialize)]
#[serde(untagged, rename_all = "lowercase")]
pub enum LPrimitive {
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
    Interrupt,
    Interruptible,
    Uninterruptible,
    Err,
    Enr,
    //QuasiInterruptible,
    Race,
}

impl Display for LPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            LPrimitive::Define => DEFINE,
            LPrimitive::DefLambda => FN_LAMBDA,
            LPrimitive::If => IF,
            LPrimitive::Quote => QUOTE,
            LPrimitive::QuasiQuote => QUASI_QUOTE,
            LPrimitive::UnQuote => UNQUOTE,
            LPrimitive::DefMacro => DEF_MACRO,
            LPrimitive::Begin => BEGIN,
            LPrimitive::Async => ASYNC,
            LPrimitive::Await => AWAIT,
            LPrimitive::Eval => EVAL,
            LPrimitive::Expand => EXPAND,
            LPrimitive::Parse => PARSE,
            LPrimitive::Do => DO,
            LPrimitive::Interrupt => INTERRUPT,
            LPrimitive::Interruptible => INTERRUPTIBLE,
            LPrimitive::Uninterruptible => UNINTERRUPTIBLE,
            LPrimitive::Race => RACE,
            LPrimitive::Enr => ENR,
            LPrimitive::Err => ERR,
        };

        write!(f, "{}", str)
    }
}

impl From<LPrimitive> for RefLValue {
    fn from(co: LPrimitive) -> Self {
        LValue::from(co).into_ref()
    }
}

impl TryFrom<&str> for LPrimitive {
    type Error = LRuntimeError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            DEFINE => Ok(LPrimitive::Define),
            FN_LAMBDA => Ok(LPrimitive::DefLambda),
            IF => Ok(LPrimitive::If),
            QUOTE => Ok(LPrimitive::Quote),
            QUASI_QUOTE => Ok(LPrimitive::QuasiQuote),
            UNQUOTE => Ok(LPrimitive::UnQuote),
            DEF_MACRO => Ok(LPrimitive::DefMacro),
            BEGIN => Ok(LPrimitive::Begin),
            ASYNC => Ok(LPrimitive::Async),
            AWAIT => Ok(LPrimitive::Await),
            EVAL => Ok(LPrimitive::Eval),
            PARSE => Ok(LPrimitive::Parse),
            EXPAND => Ok(LPrimitive::Expand),
            DO => Ok(LPrimitive::Do),
            INTERRUPT => Ok(LPrimitive::Interrupt),
            INTERRUPTIBLE | INTERRUPTIBLE_SHORT => Ok(LPrimitive::Interruptible),
            UNINTERRUPTIBLE | UNINTERRUPTIBLE_SHORT => Ok(LPrimitive::Uninterruptible),
            RACE => Ok(LPrimitive::Race),
            ENR => Ok(LPrimitive::Enr),
            ERR => Ok(LPrimitive::Err),
            //QUASI_INTERRUPTIBLE => Ok(LCoreOperator::QuasiInterruptible),
            _ => Err(LRuntimeError::new(
                "LCoreOperator::TryFrom<str>",
                "string does not correspond to core operator".to_string(),
            )),
        }
    }
}
