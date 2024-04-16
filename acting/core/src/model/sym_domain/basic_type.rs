use crate::model::sym_domain::basic_type::BasicType::*;
use crate::model::sym_domain::TypeId;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

pub const TYPE_ID_EMPTY: usize = 0;
pub const TYPE_ID_ANY: usize = 1;
pub const TYPE_ID_BOOLEAN: usize = 2;
pub const TYPE_ID_LIST: usize = 3;
pub const TYPE_ID_MAP: usize = 4;
pub const TYPE_ID_ERR: usize = 5;
pub const TYPE_ID_HANDLE: usize = 6;
pub const TYPE_ID_NUMBER: usize = 7;
pub const TYPE_ID_NIL: usize = 8;
pub const TYPE_ID_FLOAT: usize = 9;
pub const TYPE_ID_INT: usize = 10;
pub const TYPE_ID_SYMBOL: usize = 11;
pub const TYPE_ID_EMPTY_LIST: usize = 12;
pub const TYPE_ID_TRUE: usize = 13;
pub const TYPE_ID_FALSE: usize = 14;
pub const TYPE_ID_PROC: usize = 15;
pub const TYPE_ID_PRIMITIVE: usize = 16;
pub const TYPE_ID_FN: usize = 17;
pub const TYPE_ID_LAMBDA: usize = 18;
pub const TYPE_ID_VECTOR: usize = 19;
pub const TYPE_ID_TUPLE: usize = 20;

pub const TYPE_EMPTY: &str = "Empty";
pub const TYPE_ANY: &str = "Any";
pub const TYPE_BOOLEAN: &str = "Boolean";
pub const TYPE_BOOL: &str = "Bool";
pub const TYPE_LIST: &str = "List";
pub const TYPE_MAP: &str = "Map";
pub const TYPE_ERR: &str = "Err";
pub const TYPE_HANDLE: &str = "Handle";
pub const TYPE_NUMBER: &str = "Number";
pub const TYPE_NIL: &str = "Nil";
pub const TYPE_INT: &str = "Int";
pub const TYPE_FLOAT: &str = "Float";
pub const TYPE_SYMBOL: &str = "Symbol";
pub const TYPE_EMPTY_LIST: &str = "Emptylist";
pub const TYPE_TRUE: &str = "True";
pub const TYPE_FALSE: &str = "False";
pub const TYPE_PROC: &str = "Proc";
pub const TYPE_PRIMITIVE: &str = "Primitive";
pub const TYPE_FN: &str = "Fn";
pub const TYPE_LAMBDA: &str = "Lambda";
pub const TYPE_VECTOR: &str = "Vector";
pub const TYPE_TUPLE: &str = "Tuple";

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum BasicType {
    Empty = TYPE_ID_EMPTY as u8,
    Any = TYPE_ID_ANY as u8,
    Boolean = TYPE_ID_BOOLEAN as u8,
    List = TYPE_ID_LIST as u8,
    Map = TYPE_ID_MAP as u8,
    Err = TYPE_ID_ERR as u8,
    Handle = TYPE_ID_HANDLE as u8,
    Number = TYPE_ID_NUMBER as u8,
    Nil = TYPE_ID_NIL as u8,
    Int = TYPE_ID_INT as u8,
    Float = TYPE_ID_FLOAT as u8,
    Symbol = TYPE_ID_SYMBOL as u8,
    EmptyList = TYPE_ID_EMPTY_LIST as u8,
    True = TYPE_ID_TRUE as u8,
    False = TYPE_ID_FALSE as u8,
    Proc = TYPE_ID_PROC as u8,
    Primitive = TYPE_ID_PRIMITIVE as u8,
    Fn = TYPE_ID_FN as u8,
    Lambda = TYPE_ID_LAMBDA as u8,
    Vector = TYPE_ID_VECTOR as u8,
    Tuple = TYPE_ID_TUPLE as u8,
}

impl TryFrom<TypeId> for BasicType {
    type Error = ();

    fn try_from(value: TypeId) -> Result<Self, Self::Error> {
        Ok(match value {
            TYPE_ID_EMPTY => Empty,
            TYPE_ID_ANY => Any,
            TYPE_ID_BOOLEAN => Boolean,
            TYPE_ID_LIST => List,
            TYPE_ID_MAP => Map,
            TYPE_ID_ERR => Err,
            TYPE_ID_HANDLE => Handle,
            TYPE_ID_NUMBER => Number,
            TYPE_ID_NIL => Nil,
            TYPE_ID_FLOAT => Float,
            TYPE_ID_INT => Int,
            TYPE_ID_SYMBOL => Symbol,
            TYPE_ID_EMPTY_LIST => EmptyList,
            TYPE_ID_TRUE => True,
            TYPE_ID_FALSE => False,
            TYPE_ID_PROC => Proc,
            TYPE_ID_PRIMITIVE => Primitive,
            TYPE_ID_FN => Fn,
            TYPE_ID_LAMBDA => Lambda,
            TYPE_ID_VECTOR => Vector,
            TYPE_ID_TUPLE => Tuple,
            _ => return Result::Err(()),
        })
    }
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "{TYPE_ANY}"),
            Boolean => write!(f, "{TYPE_BOOLEAN}"),
            List => write!(f, "{TYPE_LIST}"),
            Empty => write!(f, "{TYPE_EMPTY}"),
            Map => write!(f, "{TYPE_MAP}"),
            Err => write!(f, "{TYPE_ERR}"),
            Handle => write!(f, "{TYPE_HANDLE}"),
            Number => write!(f, "{TYPE_NUMBER}"),
            Int => write!(f, "{TYPE_INT}"),
            Float => write!(f, "{TYPE_FLOAT}"),
            Symbol => write!(f, "{TYPE_SYMBOL}"),
            EmptyList => write!(f, "{TYPE_EMPTY_LIST}"),
            True => write!(f, "{TYPE_TRUE}"),
            False => write!(f, "{TYPE_FALSE}"),
            Nil => write!(f, "{TYPE_NIL}"),
            Proc => write!(f, "{TYPE_PROC}"),
            Primitive => write!(f, "{TYPE_PRIMITIVE}"),
            Fn => write!(f, "{TYPE_FN}"),
            Lambda => write!(f, "{TYPE_LAMBDA}"),
            Vector => write!(f, "{TYPE_VECTOR}"),
            Tuple => write!(f, "{TYPE_TUPLE}"),
        }
    }
}
