use crate::model::sym_domain::basic_type::{
    BasicType, TYPE_ID_ANY, TYPE_ID_FALSE, TYPE_ID_NIL, TYPE_ID_TRUE,
};
use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_domain::Domain::*;
use crate::ompas::manager::acting::acting_var::AsCst;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

pub mod basic_type;
pub mod cst;
pub mod domain_test;
pub mod ref_type_lattice;
pub mod simple_type;
pub mod type_lattice;

pub type TypeId = usize;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Domain {
    Simple(TypeId),
    Composed(TypeId, Vec<Domain>),
    Union(Vec<Domain>),
    Substract(Box<Domain>, Box<Domain>),
    Cst(Box<Domain>, cst::Cst),
    Application(Box<Domain>, Vec<Domain>, Box<Domain>),
}

impl Domain {
    pub fn format(&self, dc: &TypeLattice) -> String {
        match self {
            Simple(id) => dc.types[*id].to_string(),
            Composed(id, vec) => {
                format!("{}<{}>", dc.types[*id], {
                    let mut str = "".to_string();
                    for (i, d) in vec.iter().enumerate() {
                        if i != 0 {
                            str.push(',');
                        }
                        write!(str, "{}", d.format(dc)).unwrap();
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
            Application(t, params, r) => {
                let mut str = "(".to_string();
                for (i, d) in params.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{}", d.format(dc)).unwrap();
                }
                str.push(')');
                format!("{}:{str} -> {}", t.format(dc), r.format(dc))
            }
        }
    }

    pub fn flat(&mut self, tl: &TypeLattice) {
        match self {
            Simple(s) => {
                if let Some(alias) = tl.aliases.get(s) {
                    *s = *alias
                }
            }
            Composed(s, vec) => {
                if let Some(alias) = tl.aliases.get(s) {
                    *s = *alias
                }
                vec.iter_mut().for_each(|s| s.flat(tl));
            }
            Union(set) => {
                set.iter_mut().for_each(|s| s.flat(tl));
            }
            Substract(d, s) => {
                d.flat(tl);
                s.flat(tl);
            }
            Cst(t, _) => {
                t.flat(tl);
            }
            Application(a, args, r) => {
                a.flat(tl);
                args.iter_mut().for_each(|s| s.flat(tl));
                r.flat(tl);
            }
        }
    }

    pub fn get_type(&self) -> Domain {
        match self {
            Cst(t, _) => t.deref().clone(),
            _ => self.clone(),
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Cst(_, _) | Simple(TYPE_ID_TRUE | TYPE_ID_NIL | TYPE_ID_FALSE)
        )
    }

    pub fn constant(t: Domain, c: cst::Cst) -> Self {
        Cst(Box::new(t), c)
    }

    pub fn is_empty(&self) -> bool {
        self == &Simple(0)
    }

    pub fn is_application(&self) -> bool {
        matches!(self, Self::Application(_, _, _))
    }

    pub fn is_any(&self) -> bool {
        self == &Simple(1)
    }

    pub fn any() -> Self {
        Domain::Simple(1)
    }

    pub fn is_true(&self) -> bool {
        self == &Simple(TYPE_ID_TRUE)
    }

    pub fn is_false(&self) -> bool {
        self == &Simple(TYPE_ID_FALSE)
    }

    pub fn empty() -> Self {
        Simple(0)
    }

    pub fn composed(t: TypeId, mut composition: Vec<impl Into<Domain>>) -> Domain {
        Composed(t, composition.drain(..).map(|t| t.into()).collect())
    }

    pub fn nil() -> Domain {
        Union(vec![BasicType::False.into(), BasicType::EmptyList.into()])
    }
}

impl AsCst for Domain {
    fn as_cst(&self) -> Option<cst::Cst> {
        if let Self::Cst(_, cst) = &self {
            Some(cst.clone())
        } else {
            None
        }
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
                    write!(str, "{d}").unwrap();
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
            Application(t, params, r) => {
                let mut str = "(".to_string();
                for (i, d) in params.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{d}")?;
                }
                str.push(')');
                write!(f, "{t}:{str} -> {r}")
            }
        }
    }
}

impl Default for Domain {
    fn default() -> Self {
        Simple(TYPE_ID_ANY)
    }
}

impl From<BasicType> for Domain {
    fn from(r: BasicType) -> Self {
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
        Cst(Box::new(BasicType::Int.into()), cst::Cst::Int(i))
    }
}

impl From<f64> for Domain {
    fn from(f: f64) -> Self {
        Cst(Box::new(BasicType::Float.into()), cst::Cst::Float(f))
    }
}

impl From<String> for Domain {
    fn from(s: String) -> Self {
        Cst(Box::new(BasicType::Symbol.into()), cst::Cst::Symbol(s))
    }
}

impl From<&str> for Domain {
    fn from(s: &str) -> Self {
        Cst(
            Box::new(BasicType::Symbol.into()),
            cst::Cst::Symbol(s.to_string()),
        )
    }
}

impl From<bool> for Domain {
    fn from(b: bool) -> Self {
        match b {
            true => Simple(TYPE_ID_TRUE),
            false => Domain::nil(),
        }
    }
}
