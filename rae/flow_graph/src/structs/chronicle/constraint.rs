use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable, GetVariables, Replace};
use im::HashSet;
use std::fmt::Write;
#[derive(Clone, Debug)]
pub enum Constraint {
    Await(Lit),
    Not(Lit),
    Leq(Lit, Lit),
    Eq(Lit, Lit),
    Neq(Lit, Lit),
    Lt(Lit, Lit),
    Type(Lit, Lit),
    Arbitrary(Lit, Lit),
    Min(Vec<Lit>),
    Max(Vec<Lit>),
    And(Vec<Lit>),
    Or(Vec<Lit>),
}

impl Constraint {
    pub fn eq(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Eq(a.into(), b.into())
    }
    pub fn leq(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Leq(a.into(), b.into())
    }
    pub fn lt(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Lt(a.into(), b.into())
    }
    pub fn neq(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Neq(a.into(), b.into())
    }
    pub fn neg(a: impl Into<Lit>) -> Constraint {
        Constraint::Not(a.into())
    }
    pub fn or(mut vec: Vec<impl Into<Lit>>) -> Constraint {
        Constraint::Or(vec.drain(..).map(|e| e.into()).collect())
    }
    pub fn and(mut vec: Vec<impl Into<Lit>>) -> Constraint {
        Constraint::And(vec.drain(..).map(|e| e.into()).collect())
    }

    pub fn min(mut vec: Vec<impl Into<Lit>>) -> Constraint {
        Constraint::Min(vec.drain(..).map(|e| e.into()).collect())
    }
    pub fn max(mut vec: Vec<impl Into<Lit>>) -> Constraint {
        Constraint::Max(vec.drain(..).map(|e| e.into()).collect())
    }
    pub fn _type(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Type(a.into(), b.into())
    }
    pub fn arbitrary(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Arbitrary(a.into(), b.into())
    }
}

impl Constraint {
    pub fn get_left(&self) -> Lit {
        match self {
            Constraint::Leq(l1, _)
            | Constraint::Eq(l1, _)
            | Constraint::Lt(l1, _)
            | Constraint::Type(l1, _)
            | Constraint::Neq(l1, _)
            | Constraint::Arbitrary(l1, _) => l1.clone(),
            Constraint::Not(l) => l.clone(),
            Constraint::Await(a) => a.clone(),
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => vec.into(),
        }
    }

    pub fn get_right(&self) -> Lit {
        match self {
            Constraint::Leq(_, l2)
            | Constraint::Eq(_, l2)
            | Constraint::Lt(_, l2)
            | Constraint::Type(_, l2)
            | Constraint::Neq(_, l2)
            | Constraint::Arbitrary(_, l2) => l2.clone(),
            Constraint::Not(l) => l.clone(),
            Constraint::Await(a) => a.clone(),
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => vec.into(),
        }
    }
}

impl GetVariables for Constraint {
    fn get_variables(&self) -> im::HashSet<AtomId> {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Arbitrary(l1, l2) => l1.get_variables().union(l2.get_variables()),
            Constraint::And(vec)
            | Constraint::Or(vec)
            | Constraint::Max(vec)
            | Constraint::Min(vec) => {
                let mut vars: im::HashSet<AtomId> = Default::default();
                for lit in vec {
                    vars = vars.union(lit.get_variables())
                }
                vars
            }
            Constraint::Not(l) | Constraint::Await(l) => l.get_variables(),
        }
    }

    fn get_variables_of_type(
        &self,
        sym_table: &RefSymTable,
        atom_type: &AtomType,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v) == *atom_type)
            .cloned()
            .collect()
    }
}

impl FormatWithSymTable for Constraint {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            Constraint::Eq(l1, l2) => format!(
                "({} = {})",
                l1.format(st, sym_version),
                l2.format(st, sym_version)
            ),
            Constraint::Not(l1) => format!("(! {})", l1.format(st, sym_version)),
            Constraint::Lt(l1, l2) => format!(
                "({} < {})",
                l1.format(st, sym_version),
                l2.format(st, sym_version)
            ),

            Constraint::Leq(l1, l2) => {
                format!(
                    "({} <= {})",
                    l1.format(st, sym_version),
                    l2.format(st, sym_version)
                )
            }
            Constraint::Type(l1, l2) => {
                format!(
                    "(type({}) = {})",
                    l1.format(st, sym_version),
                    l2.format(st, sym_version)
                )
            }
            Constraint::Arbitrary(l1, l2) => {
                format!(
                    "({} in {})",
                    l1.format(st, sym_version),
                    l2.format(st, sym_version)
                )
            }
            Constraint::Neq(l1, l2) => format!(
                "({} != {})",
                l1.format(st, sym_version),
                l2.format(st, sym_version)
            ),
            Constraint::Await(a) => {
                format!("await({})", a.format(st, sym_version))
            }
            Constraint::Min(vec) => {
                let mut str = "".to_string();
                let mut first = true;
                for lit in vec {
                    if first {
                        write!(str, "{}", lit.format(st, sym_version)).unwrap();
                        first = false;
                    } else {
                        write!(str, ",{}", lit.format(st, sym_version)).unwrap();
                    }
                }
                format!("min({})", str)
            }
            Constraint::Max(vec) => {
                let mut str = "".to_string();
                let mut first = true;
                for lit in vec {
                    if first {
                        write!(str, "{}", lit.format(st, sym_version)).unwrap();
                        first = false;
                    } else {
                        write!(str, ",{}", lit.format(st, sym_version)).unwrap();
                    }
                }
                format!("max({})", str)
            }
            Constraint::And(vec) => {
                let mut str = "".to_string();
                let mut first = true;
                for lit in vec {
                    if first {
                        write!(str, "{}", lit.format(st, sym_version)).unwrap();
                        first = false;
                    } else {
                        write!(str, ",{}", lit.format(st, sym_version)).unwrap();
                    }
                }
                format!("and({})", str)
            }
            Constraint::Or(vec) => {
                let mut str = "".to_string();
                let mut first = true;
                for lit in vec {
                    if first {
                        write!(str, "{}", lit.format(st, sym_version)).unwrap();
                        first = false;
                    } else {
                        write!(str, ",{}", lit.format(st, sym_version)).unwrap();
                    }
                }
                format!("or({})", str)
            }
        }
    }
}

impl FlatBindings for Constraint {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Arbitrary(l1, l2) => {
                l1.flat_bindings(st);
                l2.flat_bindings(st);
            }
            Constraint::Not(a) | Constraint::Await(a) => a.flat_bindings(st),

            Constraint::And(a) | Constraint::Or(a) | Constraint::Min(a) | Constraint::Max(a) => {
                a.flat_bindings(st);
            }
        }
    }
}

impl Constraint {}

//INTERVAL constraints

pub fn before(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Leq(a.get_end().into(), b.get_start().into())
}
pub fn meet(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.get_end().into(), b.get_start().into())
}

pub fn overlap(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Or(vec![
        Lit::from(Constraint::Leq(a.get_start().into(), b.get_end().into())),
        Lit::from(Constraint::Leq(b.get_start().into(), a.get_end().into())),
    ])
}

pub fn start(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.get_start().into(), b.get_start().into())
}

pub fn during(a: &Interval, b: &Interval) -> Constraint {
    Constraint::And(vec![
        Lit::from(Constraint::Leq(a.get_start().into(), b.get_end().into())),
        Lit::from(Constraint::Leq(b.get_start().into(), a.get_end().into())),
    ])
}

pub fn finish(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.get_end().into(), b.get_end().into())
}

pub fn equal(a: &Interval, b: &Interval) -> Constraint {
    Constraint::And(vec![
        Lit::from(Constraint::Leq(a.get_start().into(), b.get_end().into())),
        Lit::from(Constraint::Leq(b.get_start().into(), a.get_end().into())),
    ])
}

impl Replace for Constraint {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Arbitrary(l1, l2) => {
                l1.replace(old, new);
                l2.replace(old, new);
            }
            Constraint::And(vec)
            | Constraint::Or(vec)
            | Constraint::Max(vec)
            | Constraint::Min(vec) => {
                vec.replace(old, new);
            }
            Constraint::Not(l) | Constraint::Await(l) => l.replace(old, new),
        }
    }
}
