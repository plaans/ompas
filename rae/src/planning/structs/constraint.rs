use crate::planning::point_algebra::problem::Relation;
use crate::planning::point_algebra::relation_type::RelationType;
use crate::planning::structs::expression_chronicle::ExpressionChronicle;
use crate::planning::structs::interval::Interval;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::PlanningAtomType;
use im::HashSet;
use ompas_lisp::core::structs::lerror::LError;
use std::convert::TryInto;

#[derive(Clone, Debug)]
pub enum Constraint {
    LEq(Lit, Lit),
    Eq(Lit, Lit),
    Neg(Lit),
    LT(Lit, Lit),
    And(Lit, Lit),
    Or(Lit, Lit),
    Type(Lit, Lit),
    Arbitrary(Lit, Lit),
}
impl Constraint {
    pub fn get_left(&self) -> &Lit {
        match self {
            Constraint::LEq(l1, _)
            | Constraint::Eq(l1, _)
            | Constraint::LT(l1, _)
            | Constraint::And(l1, _)
            | Constraint::Or(l1, _)
            | Constraint::Type(l1, _)
            | Constraint::Arbitrary(l1, _) => l1,
            Constraint::Neg(l) => l,
        }
    }

    pub fn get_right(&self) -> &Lit {
        match self {
            Constraint::LEq(_, l2)
            | Constraint::Eq(_, l2)
            | Constraint::LT(_, l2)
            | Constraint::And(_, l2)
            | Constraint::Or(_, l2)
            | Constraint::Type(_, l2)
            | Constraint::Arbitrary(_, l2) => l2,
            Constraint::Neg(l) => l,
        }
    }
}

impl GetVariables for Constraint {
    fn get_variables(&self) -> im::HashSet<AtomId> {
        match self {
            Constraint::LEq(l1, l2)
            | Constraint::Eq(l1, l2)
            | Constraint::LT(l1, l2)
            | Constraint::And(l1, l2)
            | Constraint::Or(l1, l2)
            | Constraint::Type(l1, l2)
            | Constraint::Arbitrary(l1, l2) => l1.get_variables().union(l2.get_variables()),
            Constraint::Neg(l) => l.get_variables(),
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
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Constraint::Eq(l1, l2) => format!(
                "({} = {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::Neg(l1) => format!("(! {})", l1.format_with_sym_table(st)),
            Constraint::LT(l1, l2) => format!(
                "({} < {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::And(l1, l2) => format!(
                "({} && {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::Or(l1, l2) => format!(
                "({} || {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::LEq(l1, l2) => {
                format!(
                    "({} <= {})",
                    l1.format_with_sym_table(st),
                    l2.format_with_sym_table(st)
                )
            }
            Constraint::Type(l1, l2) => {
                format!(
                    "(type({}) = {})",
                    l1.format_with_sym_table(st),
                    l2.format_with_sym_table(st)
                )
            }
            Constraint::Arbitrary(l1, l2) => {
                format!(
                    "({} in {})",
                    l1.format_with_sym_table(st),
                    l2.format_with_sym_table(st)
                )
            }
        }
    }
}

impl Constraint {
    pub fn try_into_pa_relation(&self, sym_table: &SymTable) -> Result<Relation<AtomId>, LError> {
        let relation_type = match self {
            Constraint::Eq(_, _) => RelationType::Eq,
            Constraint::LEq(_, _) => RelationType::LEq,
            Constraint::LT(_, _) => RelationType::LT,
            _ => return Err(LError::default()),
        };

        if let Ok(i) = self.get_left().try_into() {
            let p_i = sym_table.get_parent(&i);
            if let Ok(j) = self.get_right().try_into() {
                let p_j = sym_table.get_parent(&j);
                if sym_table.get_type_of(&p_i).unwrap().a_type == Some(PlanningAtomType::Timepoint)
                {
                    if sym_table.get_type_of(&p_j).unwrap().a_type
                        == Some(PlanningAtomType::Timepoint)
                    {
                        Ok(Relation::new(*p_i, *p_j, relation_type))
                    } else {
                        Err(Default::default())
                    }
                } else {
                    Err(Default::default())
                }
            } else {
                Err(Default::default())
            }
        } else {
            Err(Default::default())
        }
    }
}

//INTERVAL constraints

pub fn before(a: &Interval, b: &Interval) -> Constraint {
    Constraint::LEq(a.end().into(), b.start().into())
}
pub fn meet(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.end().into(), b.start().into())
}

pub fn overlap(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Or(
        Constraint::LEq(a.start().into(), b.end().into()).into(),
        Constraint::LEq(b.start().into(), a.end().into()).into(),
    )
}

pub fn start(a: &Interval, b: &Interval) -> Constraint {
    Constraint::Eq(a.start().into(), b.start().into())
}

pub fn during(a: &Interval, b: &Interval) -> Constraint {
    Constraint::And(
        Constraint::LEq(b.start().into(), a.start().into()).into(),
        Constraint::LEq(a.end().into(), b.end().into()).into(),
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
