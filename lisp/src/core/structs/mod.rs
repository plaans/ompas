use crate::core::structs::lerror::LError;
use crate::core::structs::lvalue::LValue;

pub mod contextcollection;
pub mod function;
pub mod lcoreoperator;
pub mod lenv;
pub mod lerror;
pub mod lfuture;
pub mod llambda;
pub mod lnumber;
pub mod lvalue;
pub mod lvalues;
pub mod module;
pub mod typelvalue;

pub type LResult = Result<LValue, LError>;
