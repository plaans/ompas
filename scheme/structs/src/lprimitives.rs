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
pub enum LPrimitives {
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
    Enr,
    //QuasiInterruptible,
    Race,
}

impl Display for LPrimitives {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            LPrimitives::Define => DEFINE,
            LPrimitives::DefLambda => LAMBDA,
            LPrimitives::If => IF,
            LPrimitives::Quote => QUOTE,
            LPrimitives::QuasiQuote => QUASI_QUOTE,
            LPrimitives::UnQuote => UNQUOTE,
            LPrimitives::DefMacro => DEF_MACRO,
            LPrimitives::Begin => BEGIN,
            LPrimitives::Async => ASYNC,
            LPrimitives::Await => AWAIT,
            LPrimitives::Eval => EVAL,
            LPrimitives::Expand => EXPAND,
            LPrimitives::Parse => PARSE,
            LPrimitives::Do => DO,
            LPrimitives::Interrupt => INTERRUPT,
            LPrimitives::Interruptible => INTERRUPTIBLE,
            LPrimitives::Uninterruptible => UNINTERRUPTIBLE,
            LPrimitives::Race => RACE,
            LPrimitives::Enr => ENR,
        };

        write!(f, "{}", str)
    }
}

impl From<LPrimitives> for RefLValue {
    fn from(co: LPrimitives) -> Self {
        LValue::from(co).into_ref()
    }
}

impl TryFrom<&str> for LPrimitives {
    type Error = LRuntimeError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            DEFINE => Ok(LPrimitives::Define),
            LAMBDA => Ok(LPrimitives::DefLambda),
            IF => Ok(LPrimitives::If),
            QUOTE => Ok(LPrimitives::Quote),
            QUASI_QUOTE => Ok(LPrimitives::QuasiQuote),
            UNQUOTE => Ok(LPrimitives::UnQuote),
            DEF_MACRO => Ok(LPrimitives::DefMacro),
            BEGIN => Ok(LPrimitives::Begin),
            ASYNC => Ok(LPrimitives::Async),
            AWAIT => Ok(LPrimitives::Await),
            EVAL => Ok(LPrimitives::Eval),
            PARSE => Ok(LPrimitives::Parse),
            EXPAND => Ok(LPrimitives::Expand),
            DO => Ok(LPrimitives::Do),
            INTERRUPT => Ok(LPrimitives::Interrupt),
            INTERRUPTIBLE | INTERRUPTIBLE_SHORT => Ok(LPrimitives::Interruptible),
            UNINTERRUPTIBLE | UNINTERRUPTIBLE_SHORT => Ok(LPrimitives::Uninterruptible),
            RACE => Ok(LPrimitives::Race),
            ENR => Ok(LPrimitives::Enr),
            //QUASI_INTERRUPTIBLE => Ok(LCoreOperator::QuasiInterruptible),
            _ => Err(LRuntimeError::new(
                "LCoreOperator::TryFrom<str>",
                "string does not correspond to core operator".to_string(),
            )),
        }
    }
}
