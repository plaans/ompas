use aries_planning::parsing::sexpr::SExpr;
use serde::{Deserialize, Serialize, Serializer};
use std::fmt::{Display, Formatter};
use ompas_lisp::structs::{LValue, LNumber};
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GodotMessageType {
    Static,
    Dynamic,
}

impl Display for GodotMessageType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            GodotMessageType::Static => write!(f, "static"),
            GodotMessageType::Dynamic => write!(f, "dynamic"),
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
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GodotStateS {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: LValueSerde,
}

impl From<&GodotStateS> for GodotState {
    fn from(gss: &GodotStateS) -> Self {
        Self {
            _type : gss._type.clone(),
            data: gss.data.borrow().into(),
        }
    }
}

impl From<GodotStateS> for GodotState {
    fn from(gss: GodotStateS) -> Self {
        (&gss).into()
    }
}

impl From<&GodotState> for GodotStateS {
    fn from(gs: &GodotState) -> Self {
        Self {
            _type : gs._type.clone(),
            data : gs.data.borrow().into()
        }
    }
}

impl From<GodotState> for GodotStateS {
    fn from(gs: GodotState) -> Self {
        (&gs).into()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(from = "GodotStateS", into = "GodotStateS")]
pub struct GodotState {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: LValue,
}

impl Display for GodotState {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "type: {}\ndata: {}", self._type, self.data)
    }
}

impl Display for GodotStateS {
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

impl From<&LValue> for LValueSerde {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Symbol(s) => LValueSerde::Symbol(s.clone()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => LValueSerde::Int(*i),
                LNumber::Float(f) => LValueSerde::Float(*f),
                LNumber::Usize(u) => LValueSerde::Int(*u as i64),
            }
            LValue::Fn(f) => LValueSerde::Symbol(f.get_label().to_string()),
            LValue::MutFn(f) => LValueSerde::Symbol(f.get_label().to_string()),
            LValue::Lambda(_) => LValue::Nil.into(),
            LValue::CoreOperator(co) => LValueSerde::Symbol(co.to_string()),
            LValue::Map(m) => {
                LValueSerde::Map(m.iter().map(|(k,v)| (k.into(), v.into())).collect())
            }
            LValue::List(l) => {
                LValueSerde::List(l.iter().map(|lv| lv.into()).collect())
            }
            LValue::Quote(l) => {l.deref().into()}
            LValue::True => LValueSerde::Symbol("true".to_string()),
            LValue::Nil => LValueSerde::Symbol("nil".to_string()),
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
            LValueSerde::Bool(b) => {
                match b {
                    true => LValue::True,
                    false => LValue::Nil,
                }
            }
            LValueSerde::List(l) => {
                if l.is_empty() {
                    LValue::Nil
                }else {
                    LValue::List(l.iter().map(|x| x.into()).collect())
                }
            }
            LValueSerde::Map(m) => {
                if m.is_empty() {
                    LValue::Nil
                }else {
                    let mut map:im::HashMap<LValue, LValue> = Default::default();
                    for (k,v) in m {
                        map.insert(k.into(),v.into());
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