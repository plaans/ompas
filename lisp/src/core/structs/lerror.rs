use crate::core::structs::lerror::LError::SpecialError;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;
use aries_model::lang::ConversionError;
use std::error::Error;
use std::fmt::{Display, Formatter};
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
/// use ompas_lisp::core::structs::lvalue::LValue;
/// //The conversion will success if lv is of kind LValue::Map
/// let lv = LValue::Map(Default::default());
/// let map: im::HashMap<LValue, LValue>  = lv.try_into().expect("Could not convert LValue into HashMap");
///
/// ```
/// # Note:
/// The first argument of each kind is supposed to be an explanation of where the error occurred.
/// It can be the name of the function.
#[derive(Debug, Clone)]
pub enum LError {
    WrongType(&'static str, LValue, TypeLValue, TypeLValue),
    NotInListOfExpectedTypes(&'static str, LValue, TypeLValue, Vec<TypeLValue>),
    WrongNumberOfArgument(&'static str, LValue, usize, Range<usize>),
    //ErrLoc(ErrLoc),
    UndefinedSymbol(&'static str, String),
    SpecialError(&'static str, String),
    ConversionError(&'static str, TypeLValue, TypeLValue),
}

impl Default for LError {
    fn default() -> Self {
        SpecialError("", "".to_string())
    }
}

impl Error for LError {}

impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(f_name, s, s1, s2) => {
                write!(f, "In {}, {}: Got {}, expected {}", f_name, s, s1, s2)
            }
            //LError::ErrLoc(e) => write!(f, "{}",e),
            LError::UndefinedSymbol(f_name, s) => write!(f, "In {}: {} is undefined", f_name, s),
            LError::WrongNumberOfArgument(f_name, s, g, r) => {
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
            LError::SpecialError(f_name, s) => write!(f, "In {}, {}", f_name, s),
            LError::ConversionError(f_name, s1, s2) => {
                write!(f, "In {}, Cannot convert {} into {}.", f_name, s1, s2)
            }
            LError::NotInListOfExpectedTypes(f_name, lv, typ, list_types) => {
                write!(
                    f,
                    "In {}, {}: Got {}, expected {:?}",
                    f_name, lv, typ, list_types
                )
            }
        }
    }
}

impl From<std::io::Error> for LError {
    fn from(e: std::io::Error) -> Self {
        SpecialError("std::io::Error", e.to_string())
    }
}

impl From<ConversionError> for LError {
    fn from(ce: ConversionError) -> Self {
        SpecialError("aries_model::lang::ConversionError", ce.to_string())
    }
}

pub type LResult = std::result::Result<LValue, LError>;

pub type Result<T> = std::result::Result<T, LError>;

impl From<anyhow::Error> for LError {
    fn from(e: anyhow::Error) -> Self {
        Self::SpecialError("anyhow", format!("{:?}", e))
    }
}
