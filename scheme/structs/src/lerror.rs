use std::backtrace::Backtrace;
use std::collections::VecDeque;
//use aries_model::lang::ConversionError;
use crate::lvalue::LValue;
use crate::typelvalue::KindLValue;
use anyhow::anyhow;
use im::Vector;
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
    backtrace: VecDeque<&'static str>,
    message: String,
}

impl LRuntimeError {
    pub fn chain(&mut self, context: &'static str) {
        self.backtrace.push_front(context)
    }
}
/*
WrongType(LValue, TypeLValue, TypeLValue),
    NotInListOfExpectedTypes(LValue, TypeLValue, Vec<TypeLValue>),
    WrongNumberOfArgument(LValue, usize, Range<usize>),
    //ErrLoc(ErrLoc),
    UndefinedSymbol(&'static str, String),
    Anyhow(&'static str, String),
    ConversionError(&'static str, TypeLValue, TypeLValue),
 */
impl LRuntimeError {
    pub fn new(context: &'static str, message: impl Display) -> Self {
        Self {
            backtrace: [context].into(),
            message: message.to_string(),
        }
    }

    pub fn wrong_type(context: &'static str, lv: &LValue, expected: KindLValue) -> Self {
        Self {
            backtrace: [context].into(),
            message: format!(
                "Wrong type: {} is a {}, expected {}.",
                lv,
                lv.get_kind(),
                expected
            ),
        }
    }
    pub fn wrong_number_of_args(
        context: &'static str,
        lv: &im::Vector<LValue>,
        expected: Range<usize>,
    ) -> Self {
        let r: String = if r.is_empty() {
            format!("expected {}", expected.start)
        } else if r.end == usize::MAX {
            format!("expected at least {}", expected.start)
        } else if r.start == usize::MIN {
            format!("expected at most {}", expected.end)
        } else {
            format!("expected between {} and {}", expected.start, expected.end)
        };
        Self {
            backtrace: [context].into(),
            message: format!(
                "Wrong number of args: {} is of length {}, {}.",
                LValue::from(lv),
                lv.len(),
                r
            ),
        }
    }

    pub fn not_in_list_of_expected_types(
        context: &'static str,
        lv: &LValue,
        t: Vec<KindLValue>,
    ) -> Self {
        Self {
            backtrace: [context].into(),
            message: format!(
                "Wrong type: {} is a {}, expected either one of {:#?}.",
                lv,
                lv.get_kind(),
                t
            ),
        }
    }
}

impl Error for LRuntimeError {}

impl Display for LRuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(f, "{:#?}: {}", self.backtrace, self.message)
        /*match self {
            LRuntimeError::WrongType(f_name, s, s1, s2) => {
                write!(
                    f,
                    "In {}, {}: Got {}, expected {}",
                    self.source().unwrap(),
                    s,
                    s1,
                    s2
                )
            }
            //LError::ErrLoc(e) => write!(f, "{}",e),
            LRuntimeError::UndefinedSymbol(f_name, s) => {
                write!(f, "In {}: {} is undefined", f_name, s)
            }
            LRuntimeError::WrongNumberOfArgument(f_name, s, g, r) => {
                if r.is_empty() {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected {}",
                        f_name, s, g, r.start
                    )
                } else if r.end == std::usize::MAX {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected at least {}",
                        f_name, s, g, r.start
                    )
                } else if r.start == std::usize::MIN {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected at most {}",
                        f_name, s, g, r.end
                    )
                } else {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected between {} and {}",
                        f_name, s, g, r.start, r.end
                    )
                }
            }
            LRuntimeError::Anyhow(f_name, s) => write!(f, "In {}, {}", f_name, s),
            LRuntimeError::ConversionError(f_name, s1, s2) => {
                write!(f, "In {}, Cannot convert {} into {}.", f_name, s1, s2)
            }
            LRuntimeError::NotInListOfExpectedTypes(f_name, lv, typ, list_types) => {
                write!(
                    f,
                    "In {}, {}: Got {}, expected {:?}",
                    f_name, lv, typ, list_types
                )
            }
        }*/
    }
}

impl From<anyhow::Error> for LRuntimeError {
    fn from(a: anyhow::Error) -> Self {
        Self {
            backtrace: [""].into(),
            message: a.to_string(),
        }
    }
}

impl From<std::io::Error> for LRuntimeError {
    fn from(e: std::io::Error) -> Self {
        Anyhow("std::io::Error", e.to_string())
    }
}

pub type LResult = std::Result<LValue, LRuntimeError>;

pub type Result<T> = std::result::Result<T, LRuntimeError>;
