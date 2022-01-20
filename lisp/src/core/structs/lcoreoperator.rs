use crate::core::structs::lcoreoperator::language::*;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::SpecialError;
use serde::*;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

pub mod language {

    pub const DEFINE: &str = "define";
    pub const DEF_MACRO: &str = "defmacro";
    pub const LAMBDA: &str = "lambda";
    pub const IF: &str = "if";
    pub const QUOTE: &str = "quote";
    pub const QUASI_QUOTE: &str = "quasiquote";
    pub const UNQUOTE: &str = "unquote";
    pub const QUOTE_CHAR: char = '\'';
    pub const UNQUOTE_CHAR: char = ',';
    pub const QUASI_QUOTE_CHAR: char = '`';
    pub const BEGIN: &str = "begin";
    pub const ASYNC: &str = "async";
    pub const AWAIT: &str = "await";
    pub const RACE: &str = "race";
    pub const EVAL: &str = "eval";
    pub const PARSE: &str = "parse";
    pub const EXPAND: &str = "expand";

    pub const DOC_DEFINE: &str = "Defines a new entry in the environment/";
    pub const DOC_DEF_MACRO: &str = "Define a new macro, can only be done at the top level";
    pub const DOC_LAMBDA: &str = "Define a new lambda function";
    pub const DOC_LAMBDA_VEBROSE: &str = "Example:\n\
                                    \t>> (define square (lambda (x) (* x x)))\n\
                                    \t>> (square 10)\n\
                                    \tLI>> 100";
    pub const DOC_IF: &str = "Condition block";
    pub const DOC_QUOTE: &str = "Prevent the LValue to be evaluated";
    pub const DOC_QUASI_QUOTE: &str = "Begins a context mixing quoting and unquoting";
    pub const DOC_UNQUOTE: &str = "Evaluated an expression present inside a quasi-quote statement";
    pub const DOC_BEGIN: &str = "Evaluated a list LValue and returns the last one.";
    pub const DOC_BEGIN_VERBOSE: &str = "Example: \n\
                                     \t>>(begin 10 (* 3 3))\n\
                                     \tLI>> 9";

    pub const DOC_ASYNC: &str =
        "Evaluate asynchronously a LValue. Returns the pid(usize) of the task";
    pub const DOC_AWAIT: &str = "Await on a pid to get the result of the evaluation.";
    pub const DOC_EVAL: &str = "Eval a LValue.";
}

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
#[derive(Clone, PartialOrd, PartialEq, Eq, Debug, Serialize, Deserialize)]
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
    Async,
    Await,
    Parse,
    Expand,
    Eval,
}

impl Display for LCoreOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LCoreOperator::Define => write!(f, "{}", DEFINE),
            LCoreOperator::DefLambda => write!(f, "{}", LAMBDA),
            LCoreOperator::If => write!(f, "{}", IF),
            LCoreOperator::Quote => write!(f, "{}", QUOTE),
            LCoreOperator::QuasiQuote => write!(f, "{}", QUASI_QUOTE),
            LCoreOperator::UnQuote => write!(f, "{}", UNQUOTE),
            LCoreOperator::DefMacro => write!(f, "{}", DEF_MACRO),
            LCoreOperator::Begin => write!(f, "{}", BEGIN),
            LCoreOperator::Async => write!(f, "{}", ASYNC),
            LCoreOperator::Await => write!(f, "{}", AWAIT),
            LCoreOperator::Eval => write!(f, "{}", EVAL),
            LCoreOperator::Expand => write!(f, "{}", EXPAND),
            LCoreOperator::Parse => write!(f, "{}", PARSE),
        }
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
            _ => Err(SpecialError(
                "LCoreOperator::TryFrom<str>",
                "string does not correspond to core operator".to_string(),
            )),
        }
    }
}
