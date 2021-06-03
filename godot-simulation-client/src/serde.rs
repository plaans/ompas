use aries_planning::parsing::sexpr::SExpr;
use ompas_lisp::structs::LError::WrongType;
use ompas_lisp::structs::{LError, LNumber, LValue, NameTypeLValue};
use serde::{Deserialize, Serialize, Serializer};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::state::{LState, StateType};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GodotMessageType {
    Static,
    Dynamic,
    RobotCommand,
}

impl Display for GodotMessageType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            GodotMessageType::Static => write!(f, "static"),
            GodotMessageType::Dynamic => write!(f, "dynamic"),
            GodotMessageType::RobotCommand => write!(f, "robot_command"),
        }
    }
}

impl Serialize for GodotMessageType {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self {
            GodotMessageType::Static => serializer.serialize_str("static"),
            GodotMessageType::Dynamic => serializer.serialize_str("dynamic"),
            GodotMessageType::RobotCommand => serializer.serialize_str("robot_command"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GodotMessageSerde {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: LValueSerde,
}

impl TryFrom<GodotMessageSerde> for LState {
    type Error = LError;

    fn try_from(value: GodotMessageSerde) -> Result<Self, Self::Error> {
        let mut state: LState = Default::default();
        match value._type {
            GodotMessageType::Static => state.set_type(StateType::Static),
            GodotMessageType::Dynamic => state.set_type(StateType::Dynamic),
            GodotMessageType::RobotCommand => return Err(LError::SpecialError("Was not expecting a robot command".to_string()))
        }
        match &value.data {
            LValueSerde::List(l) => {
                for e in l {
                    match e {
                        LValueSerde::List(list) => {
                            state.insert(LValueSerde::List(list[0..list.len() - 1].to_vec()), list.last().unwrap().clone());
                        }
                        lv => panic!("there should be a list")
                    }
                }
            }
            lv => return Err(WrongType(lv.into(), LValue::from(lv).into(), NameTypeLValue::List)),
        }
        Ok(state)
    }
}

impl Display for GodotMessageSerde {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "type: {}\ndata: {}", self._type, self.data)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LValueSerde {
    Symbol(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<LValueSerde>),
    Map(Vec<(LValueSerde, LValueSerde)>),
}

impl Hash for LValueSerde {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValueSerde::Symbol(s) => (*s).hash(state),
            LValueSerde::Int(i) => (*i).hash(state),
            LValueSerde::Float(f) => (*f).to_string().hash(state),
            LValueSerde::Bool(b) => b.hash(state),
            LValueSerde::Map(m) => (*m).hash(state),
            LValueSerde::List(l) => {
                (*l).hash(state);
            }
        };
    }
}

impl PartialEq for LValueSerde {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LValueSerde::Int(i1), LValueSerde::Int(i2)) => *i1 == *i2,
            (LValueSerde::Symbol(s1), LValueSerde::Symbol(s2)) => *s1 == *s2,
            (LValueSerde::Bool(b1), LValueSerde::Bool(b2)) => b1 == b2,
            (LValueSerde::Float(f1), LValueSerde::Float(f2)) => *f1 == *f2,
            (LValueSerde::List(l1), LValueSerde::List(l2)) => *l1 == *l2,
            (LValueSerde::Map(m1), LValueSerde::Map(m2)) => *m1 == *m2,
            (_,_) => false,
        }
    }
}

impl Eq for LValueSerde {

}

impl From<&LValue> for LValueSerde {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Symbol(s) => LValueSerde::Symbol(s.clone()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => LValueSerde::Int(*i),
                LNumber::Float(f) => LValueSerde::Float(*f),
                LNumber::Usize(u) => LValueSerde::Int(*u as i64),
            },
            LValue::Fn(f) => LValueSerde::Symbol(f.get_label().to_string()),
            LValue::MutFn(f) => LValueSerde::Symbol(f.get_label().to_string()),
            LValue::Lambda(_) => LValue::Nil.into(),
            LValue::CoreOperator(co) => LValueSerde::Symbol(co.to_string()),
            LValue::Map(m) => {
                LValueSerde::Map(m.iter().map(|(k, v)| (k.into(), v.into())).collect())
            }
            LValue::List(l) => LValueSerde::List(l.iter().map(|lv| lv.into()).collect()),
            LValue::Quote(l) => l.deref().into(),
            LValue::True => LValueSerde::Symbol("true".to_string()),
            LValue::Nil => LValueSerde::Symbol("nil".to_string()),
            LValue::String(s) => LValueSerde::Symbol(s.clone()),
            LValue::Character(c) => LValueSerde::Symbol(c.to_string()),
        }
    }
}

impl From<LValue> for LValueSerde {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl From<&LValueSerde> for LValue {
    fn from(lvs: &LValueSerde) -> Self {
        match lvs {
            LValueSerde::Symbol(s) => LValue::Symbol(s.clone()),
            LValueSerde::Int(i) => LValue::Number(LNumber::Int(*i)),
            LValueSerde::Float(f) => LValue::Number(LNumber::Float(*f)),
            LValueSerde::Bool(b) => match b {
                true => LValue::True,
                false => LValue::Nil,
            },
            LValueSerde::List(l) => {
                if l.is_empty() {
                    LValue::Nil
                } else {
                    LValue::List(l.iter().map(|x| x.into()).collect())
                }
            }
            LValueSerde::Map(m) => {
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

impl From<LValueSerde> for LValue {
    fn from(lvs: LValueSerde) -> Self {
        (&lvs).into()
    }
}
impl Display for LValueSerde {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValueSerde::Symbol(s) => write!(f, "{}", s),
            LValueSerde::Int(i) => write!(f, "{}", *i),
            LValueSerde::Float(fl) => write!(f, "{}", fl),
            LValueSerde::Bool(b) => {
                write!(f, "{}", *b)
            }
            LValueSerde::List(l) => {
                let mut str = String::from("(");
                for e in l {
                    str.push_str(format!("{} ", e).as_str())
                }
                str.push(')');
                write!(f, "{}", str)
            }
            LValueSerde::Map(m) => {
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

#[allow(clippy::result_unit_err)]
pub fn parse_into_lvalue(se: &SExpr) -> Result<LValueSerde, ()> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValueSerde::Int(int)),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValueSerde::Float(float)),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        "true" => {
                            //println!("atom is boolean true");
                            Ok(LValueSerde::Bool(true))
                        }
                        "false" => {
                            //println!("atom is boolean false");
                            Ok(LValueSerde::Bool(false))
                        }

                        s => Ok(LValueSerde::Symbol(s.to_string())),
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let list_iter = list.iter();
            let vec: Vec<LValueSerde> = list_iter
                .map(|x| parse_into_lvalue(x))
                .collect::<Result<_, _>>()?;
            Ok(LValueSerde::List(vec))
        }
    }
}

#[cfg(test)]
mod tests {}
