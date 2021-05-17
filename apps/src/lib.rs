use aries_planning::parsing::sexpr::SExpr;
use serde::{Deserialize, Serialize, Serializer};
use std::fmt::{Display, Formatter};

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
pub struct GodotState {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: LValueS,
}
impl Display for GodotState {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "type: {}\ndata: {}", self._type, self.data)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LValueS {
    Sym(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<LValueS>),
}

impl Display for LValueS {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValueS::Sym(s) => write!(f, "{}", s),
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
        }
    }
}

#[allow(clippy::result_unit_err)]
pub fn parse_into_lvalue(se: &SExpr) -> Result<LValueS, ()> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValueS::Int(int)),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValueS::Float(float)),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        "true" => {
                            //println!("atom is boolean true");
                            Ok(LValueS::Bool(true))
                        }
                        "false" => {
                            //println!("atom is boolean false");
                            Ok(LValueS::Bool(false))
                        }

                        s => Ok(LValueS::Sym(s.to_string())),
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let list_iter = list.iter();
            let vec: Vec<LValueS> = list_iter
                .map(|x| parse_into_lvalue(x))
                .collect::<Result<_, _>>()?;
            Ok(LValueS::List(vec))
        }
    }
}
