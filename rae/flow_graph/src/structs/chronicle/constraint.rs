use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables, Replace, VarId};
use crate::structs::domain::Domain;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use im::HashSet;
use std::fmt::Write;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
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
    Add(Lit, Lit, Lit),
    Sub(Lit, Lit, Lit),
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
    pub fn not(a: impl Into<Lit>) -> Constraint {
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

    pub fn add(mut a: impl Into<Lit>, b: impl Into<Lit>, c: impl Into<Lit>) -> Constraint {
        Constraint::Add(a.into(), b.into(), c.into())
    }

    pub fn sub(mut a: impl Into<Lit>, b: impl Into<Lit>, c: impl Into<Lit>) -> Constraint {
        Constraint::Sub(a.into(), b.into(), c.into())
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
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => vec.into(),
            Constraint::Add(a, _, _) | Constraint::Sub(a, _, _) => a.clone(),
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
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => vec.into(),

            Constraint::Add(_, b, _) | Constraint::Sub(_, b, _) => b.clone(),
        }
    }
}

impl GetVariables for Constraint {
    fn get_variables(&self) -> im::HashSet<VarId> {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Arbitrary(l1, l2) => l1.get_variables().union(l2.get_variables()),
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => {
                let mut vars: im::HashSet<VarId> = Default::default();
                for lit in vec {
                    vars = vars.union(lit.get_variables())
                }
                vars
            }
            Constraint::Add(a, b, c) | Constraint::Sub(a, b, c) => a
                .get_variables()
                .union(b.get_variables().union(c.get_variables())),
            Constraint::Not(l) => l.get_variables(),
        }
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), &domain))
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
            Constraint::Add(a, b, c) => {
                format!(
                    "{} = {} + {}",
                    a.format(st, sym_version),
                    b.format(st, sym_version),
                    c.format(st, sym_version)
                )
            }
            Constraint::Sub(a, b, c) => {
                format!(
                    "{} = {} - {}",
                    a.format(st, sym_version),
                    b.format(st, sym_version),
                    c.format(st, sym_version)
                )
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
            Constraint::Not(a) => a.flat_bindings(st),

            Constraint::Min(a) | Constraint::Max(a) | Constraint::And(a) | Constraint::Or(a) => {
                a.flat_bindings(st)
            }
            Constraint::Add(a, b, c) | Constraint::Sub(a, b, c) => {
                a.flat_bindings(st);
                b.flat_bindings(st);
                c.flat_bindings(st);
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
    fn replace(&mut self, old: &VarId, new: &VarId) {
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
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => {
                vec.replace(old, new);
            }

            Constraint::Add(a, b, c) | Constraint::Sub(a, b, c) => {
                a.replace(old, new);
                b.replace(old, new);
                c.replace(old, new);
            }
            Constraint::Not(l) => l.replace(old, new),
        }
    }
}
