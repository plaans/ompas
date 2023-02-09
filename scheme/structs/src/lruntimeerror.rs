use crate::kindlvalue::KindLValue;
use crate::lvalue::LValue;
use anyhow::anyhow;
use std::error::Error;
use std::fmt::{format, Display, Formatter};
use std::ops::Range;

/// Error struct for Scheme
/// Different kinds of errors are proposed, to have better explanation when one occurs:
/// - WrongType: the LValue kind is not the one expected
/// - NotInListOfExpectedTypes: a list of kind of LValue was expected.
/// - WrongNumberOfArgument: the number of args is not in expected range
/// - SpecialError: Other types of errors
/// - ConversionError: Error when trying a conversion that could fail.
/// # Example:
/// ```
/// use std::convert::TryInto;
/// use sompas_structs::lvalue::LValue;
/// //The conversion will success if lv is of kind LValue::Map
/// let lv = LValue::Map(Default::default());
/// let map: im::HashMap<LValue, LValue>  = lv.try_into().expect("Could not convert LValue into HashMap");
///
/// ```
/// # Note:
/// The first argument of each kind is supposed to be an explanation of where the error occurred.
/// It can be the name of the function.
#[derive(Debug, Clone, Default)]
pub struct LRuntimeError {
    backtrace: Vec<String>,
    message: String,
}

impl LRuntimeError {
    pub fn get_message(&self) -> &str {
        &self.message
    }

    pub fn chain(mut self, context: impl Display) -> Self {
        self.backtrace.push(context.to_string());
        self
    }
}

impl LRuntimeError {
    pub fn new(context: impl Display, message: impl Display) -> Self {
        Self {
            backtrace: vec![context.to_string()],
            message: message.to_string(),
        }
    }

    pub fn wrong_type(context: impl Display, lv: &LValue, expected: KindLValue) -> Self {
        Self {
            backtrace: vec![context.to_string()],
            message: format!(
                "Wrong type: {} is a {}, expected {}.",
                lv,
                lv.get_kind(),
                expected
            ),
        }
    }
    pub fn wrong_number_of_args(
        context: impl Display,
        lv: &[LValue],
        expected: Range<usize>,
    ) -> Self {
        let r: String = if expected.is_empty() {
            format!("expected {}", expected.start)
        } else if expected.end == usize::MAX {
            format!("expected at least {}", expected.start)
        } else if expected.start == usize::MIN {
            format!("expected at most {}", expected.end)
        } else {
            format!("expected between {} and {}", expected.start, expected.end)
        };
        Self {
            backtrace: vec![context.to_string()],
            message: format!(
                "Wrong number of args: {} is of length {}, {}.",
                LValue::from(lv),
                lv.len(),
                r
            ),
        }
    }

    pub fn not_in_list_of_expected_types(
        context: impl Display,
        lv: &LValue,
        t: Vec<KindLValue>,
    ) -> Self {
        Self {
            backtrace: vec![context.to_string()],
            message: format!(
                "Wrong type: {} is a {}, expected either one of {:#?}.",
                lv,
                lv.get_kind(),
                t
            ),
        }
    }

    pub fn conversion_error(context: &'static str, lv: &LValue, kind: KindLValue) -> Self {
        Self {
            backtrace: vec![context.to_string()],
            message: format!(
                "Cannot convert {} of type {} into {}",
                lv,
                lv.get_kind(),
                kind
            ),
        }
    }

    pub fn extended_conversion_error<A: Display, B>(context: impl Display, lv: &A) -> Self {
        Self {
            backtrace: vec![context.to_string()],
            message: format!(
                "Cannot convert {} of type {} into {}",
                lv,
                std::any::type_name::<A>(),
                std::any::type_name::<B>()
            ),
        }
    }
}

impl Error for LRuntimeError {}

impl Display for LRuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(f, "message:\n{}", self.message).expect("");
        writeln!(f, "\nbacktrace:").expect("");
        for a in &self.backtrace {
            writeln!(f, "- from {}", a).expect("");
        }
        Ok(())
    }
}

impl From<anyhow::Error> for LRuntimeError {
    fn from(a: anyhow::Error) -> Self {
        Self {
            backtrace: vec!["anyhow".to_string()],
            message: format!("{:?}", a),
        }
    }
}

impl From<std::io::Error> for LRuntimeError {
    fn from(e: std::io::Error) -> Self {
        Self {
            backtrace: vec!["std::io::Error".to_string()],
            message: format!("{:?}", e),
        }
    }
}

impl From<LValue> for LResult {
    fn from(lv: LValue) -> Self {
        Ok(lv)
    }
}
pub type LResult = std::result::Result<LValue, LRuntimeError>;

pub type Result<T> = std::result::Result<T, LRuntimeError>;
