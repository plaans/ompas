use crate::structs::expression_chronicle::ExpressionChronicle;
use crate::structs::interval::Interval;
use crate::structs::lit::Lit;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::structs::type_table::PlanningAtomType;
use im::HashSet;

#[derive(Clone, Debug)]
pub enum Constraint {
    Leq(Lit, Lit),
    Eq(Lit, Lit),
    Neq(Lit, Lit),
    Not(Lit),
    Lt(Lit, Lit),
    And(Lit, Lit),
    Or(Lit, Lit),
    Type(Lit, Lit),
    Arbitrary(Lit, Lit),
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
    pub fn or(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Or(a.into(), b.into())
    }
    pub fn and(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::And(a.into(), b.into())
    }
    pub fn _type(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Type(a.into(), b.into())
    }
    pub fn arbitrary(a: impl Into<Lit>, b: impl Into<Lit>) -> Constraint {
        Constraint::Arbitrary(a.into(), b.into())
    }
}

impl Constraint {
    pub fn get_left(&self) -> &Lit {
        match self {
            Constraint::Leq(l1, _)
            | Constraint::Eq(l1, _)
            | Constraint::Lt(l1, _)
            | Constraint::And(l1, _)
            | Constraint::Or(l1, _)
            | Constraint::Type(l1, _)
            | Constraint::Neq(l1, _)
            | Constraint::Arbitrary(l1, _) => l1,
            Constraint::Not(l) => l,
        }
    }

    pub fn get_right(&self) -> &Lit {
        match self {
            Constraint::Leq(_, l2)
            | Constraint::Eq(_, l2)
            | Constraint::Lt(_, l2)
            | Constraint::And(_, l2)
            | Constraint::Or(_, l2)
            | Constraint::Type(_, l2)
            | Constraint::Neq(_, l2)
            | Constraint::Arbitrary(_, l2) => l2,
            Constraint::Not(l) => l,
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
            | Constraint::And(l1, l2)
            | Constraint::Or(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Arbitrary(l1, l2) => l1.get_variables().union(l2.get_variables()),
            Constraint::Not(l) => l.get_variables(),
        }
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}

impl FormatWithSymTable for Constraint {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
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
            Constraint::And(l1, l2) => format!(
                "({} && {})",
                l1.format(st, sym_version),
                l2.format(st, sym_version)
            ),
            Constraint::Or(l1, l2) => format!(
                "({} || {})",
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
        }
    }
}

impl FormatWithParent for Constraint {
    fn format_with_parent(&mut self, st: &SymTable) {
        match self {
            Constraint::Leq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::Lt(l1, l2)
            | Constraint::And(l1, l2)
            | Constraint::Or(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Neq(l1, l2)
            | Constraint::Arbitrary(l1, l2) => {
                l1.format_with_parent(st);
                l2.format_with_parent(st);
            }
            Constraint::Not(a) => a.format_with_parent(st),
        }
    }
}

impl Constraint {}

//INTERVAL constraints

pub fn before(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Leq(a.end().into(), b.start().into())
}
pub fn meet(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.end().into(), b.start().into())
}

pub fn overlap(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Or(
        Constraint::Leq(a.start().into(), b.end().into()).into(),
        Constraint::Leq(b.start().into(), a.end().into()).into(),
    )
}

pub fn start(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.start().into(), b.start().into())
}

pub fn during(a: &Interval, b: &Interval) -> Constraint {
    Constraint::And(
        Constraint::Leq(b.start().into(), a.start().into()).into(),
        Constraint::Leq(a.end().into(), b.end().into()).into(),
    )
}

pub fn finish(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.end().into(), b.end().into())
}

pub fn equal(a: &Interval, b: &Interval) -> Constraint {
    Constraint::And(
        Constraint::Eq(a.start().into(), b.start().into()).into(),
        Constraint::Eq(a.end().into(), b.end().into()).into(),
    )
}

pub fn bind_result(a: &ExpressionChronicle, b: &ExpressionChronicle) -> Constraint {
    Constraint::Eq(a.get_result_as_lit(), b.get_result_as_lit())
}
