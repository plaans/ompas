use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::*;
use crate::structs::domain::type_lattice::TypeLattice;
use crate::structs::domain::Domain::*;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

pub mod basic_type;
pub mod cst;
pub mod domain_test;
pub mod root_type;
pub mod type_lattice;

pub type TypeId = usize;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Domain {
    Simple(TypeId),
    Composed(TypeId, Vec<Domain>),
    Union(Vec<Domain>),
    Substract(Box<Domain>, Box<Domain>),
    Cst(Box<Domain>, cst::Cst),
}

impl Domain {
    pub fn format(&self, dc: &TypeLattice) -> String {
        match self {
            Simple(id) => dc.types[*id].to_string(),
            Composed(id, vec) => {
                format!("{}<{}>", dc.types[*id].to_string(), {
                    let mut str = "".to_string();
                    for (i, d) in vec.iter().enumerate() {
                        if i != 0 {
                            str.push(',');
                        }
                        write!(str, "{}", d.format(dc));
                    }
                    str
                })
            }
            Union(u) => {
                let mut str = "{".to_string();
                for (i, d) in u.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{}", d.format(dc)).unwrap();
                }
                str.push('}');
                str
            }
            Substract(t1, t2) => {
                format!("({} / {})", t1.format(dc), t2.format(dc))
            }
            Cst(t, c) => {
                format!("{}[{c}]", t.format(dc))
            }
        }
    }

    pub fn is_constant(&self) -> bool {
        if let Self::Cst(_, _) = &self {
            true
        } else {
            false
        }
    }

    pub fn nil() -> Domain {
        Union(vec![False.into(), EmptyList.into()])
    }
}

impl Display for Domain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Simple(id) => write!(f, "{id}"),
            Composed(id, vec) => write!(f, "{id}<{}>", {
                let mut str = "".to_string();
                for (i, d) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{d}",);
                }
                str
            }),
            Union(u) => {
                let mut str = "{".to_string();
                for (i, d) in u.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{d}")?;
                }
                str.push('}');
                write!(f, "{str}")
            }
            Substract(t1, t2) => {
                write!(f, "({t1} / {t2})")
            }
            Cst(t, c) => {
                write!(f, "{t}[{c}]")
            }
        }
    }
}

impl Default for Domain {
    fn default() -> Self {
        Simple(Any as usize)
    }
}

impl From<RootType> for Domain {
    fn from(r: RootType) -> Self {
        Simple(r as usize)
    }
}

impl From<TypeId> for Domain {
    fn from(id: TypeId) -> Self {
        Simple(id)
    }
}

impl From<&TypeId> for Domain {
    fn from(value: &TypeId) -> Self {
        Simple(*value)
    }
}

impl From<i64> for Domain {
    fn from(i: i64) -> Self {
        Cst(Box::new(Int.into()), cst::Cst::Int(i))
    }
}

impl From<f64> for Domain {
    fn from(f: f64) -> Self {
        Cst(Box::new(Float.into()), cst::Cst::Float(f))
    }
}

impl From<String> for Domain {
    fn from(s: String) -> Self {
        Cst(Box::new(Symbol.into()), cst::Cst::Symbol(s))
    }
}

impl From<bool> for Domain {
    fn from(b: bool) -> Self {
        match b {
            true => Simple(True as usize),
            false => Domain::nil(),
        }
    }
}
