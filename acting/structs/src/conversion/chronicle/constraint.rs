use crate::conversion::chronicle::interval::Interval;
use crate::sym_table::domain::Domain;
use crate::sym_table::lit::Lit;
use crate::sym_table::litset::LitSet;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::HashSet;
use std::fmt::Write;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Constraint {
    Not(Lit),
    Leq(Lit, Lit),
    Eq(Lit, Lit),
    Neq(Lit, Lit),
    Lt(Lit, Lit),
    Arbitrary(LitSet),
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
    pub fn arbitrary(a: impl Into<LitSet>) -> Constraint {
        Constraint::Arbitrary(a.into())
    }
}

impl Constraint {
    pub fn get_left(&self) -> Lit {
        match self {
            Constraint::Leq(l1, _)
            | Constraint::Eq(l1, _)
            | Constraint::Lt(l1, _)
            | Constraint::Neq(l1, _) => l1.clone(),
            Constraint::Not(l) => l.clone(),
            Constraint::Arbitrary(l) => Lit::Set(l.clone()),
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
            | Constraint::Neq(_, l2) => l2.clone(),
            Constraint::Not(l) => l.clone(),
            Constraint::Arbitrary(l) => Lit::Set(l.clone()),
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => vec.into(),
        }
    }
}

impl GetVariables for Constraint {
    fn get_variables(&self) -> im::HashSet<VarId> {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Lt(l1, l2) => l1.get_variables().union(l2.get_variables()),
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
            Constraint::Not(l) => l.get_variables(),
            Constraint::Arbitrary(l) => l.get_variables(),
        }
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
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
            Constraint::Arbitrary(l1) => {
                format!("arbitrary({})", l1.format(st, sym_version),)
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
        }
    }
}

impl FlatBindings for Constraint {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::Neq(l1, l2) => {
                l1.flat_bindings(st);
                l2.flat_bindings(st);
            }
            Constraint::Not(a) => a.flat_bindings(st),
            Constraint::Arbitrary(a) => a.flat_bindings(st),

            Constraint::Min(a) | Constraint::Max(a) | Constraint::And(a) | Constraint::Or(a) => {
                a.flat_bindings(st)
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
            | Constraint::Lt(l1, l2) => {
                l1.replace(old, new);
                l2.replace(old, new);
            }
            Constraint::Min(vec)
            | Constraint::Max(vec)
            | Constraint::And(vec)
            | Constraint::Or(vec) => {
                vec.replace(old, new);
            }
            Constraint::Not(l) => l.replace(old, new),
            Constraint::Arbitrary(l) => l.replace(old, new),
        }
    }
}
