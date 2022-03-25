use crate::core::language::ERR;
use crate::core::structs::lerror::LError;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use serde::*;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// Enum used to serialize LValue.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LValueS {
    Symbol(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    List(Vec<LValueS>),
    Map(Vec<(LValueS, LValueS)>),
}

impl Hash for LValueS {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValueS::Symbol(s) => (*s).hash(state),
            LValueS::Int(i) => (*i).hash(state),
            LValueS::Float(f) => (*f).to_string().hash(state),
            LValueS::Bool(b) => b.hash(state),
            LValueS::Map(m) => (*m).hash(state),
            LValueS::List(l) => {
                (*l).hash(state);
            }
        };
    }
}

impl PartialEq for LValueS {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LValueS::Int(i1), LValueS::Int(i2)) => *i1 == *i2,
            (LValueS::Symbol(s1), LValueS::Symbol(s2)) => *s1 == *s2,
            (LValueS::Bool(b1), LValueS::Bool(b2)) => b1 == b2,
            (LValueS::Float(f1), LValueS::Float(f2)) => *f1 == *f2,
            (LValueS::List(l1), LValueS::List(l2)) => *l1 == *l2,
            (LValueS::Map(m1), LValueS::Map(m2)) => *m1 == *m2,
            (_, _) => false,
        }
    }
}

impl Eq for LValueS {}

impl From<&LValue> for LValueS {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Symbol(s) => LValueS::Symbol(s.clone()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => LValueS::Int(*i),
                LNumber::Float(f) => LValueS::Float(*f),
            },
            LValue::Fn(f) => LValueS::Symbol(f.get_label().to_string()),
            LValue::Lambda(_) => panic!("cannot convert LValue::Lambda into LValueS"),
            LValue::CoreOperator(co) => LValueS::Symbol(co.to_string()),
            LValue::Map(m) => LValueS::Map(m.iter().map(|(k, v)| (k.into(), v.into())).collect()),
            LValue::List(l) => LValueS::List(l.iter().map(|lv| lv.into()).collect()),
            //LValue::Quote(l) => l.deref().into(),
            LValue::True => LValueS::Bool(true),
            LValue::Nil => LValueS::Bool(false),
            LValue::String(s) => LValueS::Symbol(s.clone()),
            LValue::Character(c) => LValueS::Symbol(c.to_string()),
            LValue::AsyncFn(fun) => LValueS::Symbol(fun.get_label().to_string()),
            LValue::Future(_) => panic!("cannot convert LValue::Future into LValueS"),
            LValue::Err(e) => {
                LValueS::List(vec![LValueS::Symbol(ERR.to_string()), e.deref().into()])
            }
        }
    }
}

impl From<LValue> for LValueS {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl From<&LValueS> for LValue {
    fn from(lvs: &LValueS) -> Self {
        match lvs {
            LValueS::Symbol(s) => LValue::Symbol(s.clone()),
            LValueS::Int(i) => LValue::Number(LNumber::Int(*i)),
            LValueS::Float(f) => LValue::Number(LNumber::Float(*f)),
            LValueS::Bool(b) => match b {
                true => LValue::True,
                false => LValue::Nil,
            },
            LValueS::List(l) => {
                if l.is_empty() {
                    LValue::Nil
                } else {
                    LValue::List(l.iter().map(|x| x.into()).collect())
                }
            }
            LValueS::Map(m) => {
                if m.is_empty() {
                    LValue::Nil
                } else {
                    let mut map: im::HashMap<LValue, LValue> = Default::default();
                    for (k, v) in m {
                        map.insert(k.into(), v.into());
                    }
                    LValue::Map(map)
                }
            }
        }
    }
}

impl From<&str> for LValueS {
    fn from(s: &str) -> Self {
        LValueS::Symbol(s.to_string())
    }
}

impl From<LValueS> for LValue {
    fn from(lvs: LValueS) -> Self {
        (&lvs).into()
    }
}

impl TryFrom<&LValueS> for Vec<LValueS> {
    type Error = LError;

    fn try_from(value: &LValueS) -> Result<Self, Self::Error> {
        match value {
            LValueS::List(l) => Ok(l.clone()),
            _ => Err(Default::default()),
        }
    }
}

impl TryFrom<&LValueS> for String {
    type Error = LError;

    fn try_from(value: &LValueS) -> Result<Self, Self::Error> {
        if let LValueS::Symbol(s) = value {
            Ok(s.clone())
        } else {
            Err(Default::default())
        }
    }
}

impl Display for LValueS {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValueS::Symbol(s) => write!(f, "{}", s),
            LValueS::Int(i) => write!(f, "{}", *i),
            LValueS::Float(fl) => write!(f, "{}", fl),
            LValueS::Bool(b) => {
                write!(f, "{}", *b)
            }
            LValueS::List(l) => {
                let mut str = String::from("(");
                for e in l {
                    str.push_str(format!("{} ", e).as_str())
                }
                str.push(')');
                write!(f, "{}", str)
            }
            LValueS::Map(m) => {
                let mut str = String::from("(");
                for e in m {
                    str.push_str(format!("{} . {} ", e.0, e.1).as_str())
                }
                str.push(')');
                write!(f, "{}", str)
            }
        }
    }
}
