use crate::planning::point_algebra::problem::Relation;
use crate::planning::point_algebra::relation_type::RelationType;
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
use ompas_lisp::core::structs::lerror::LError;
use std::convert::TryInto;

#[derive(Clone)]
pub enum Constraint {
    LEq(Lit, Lit),
    Eq(Lit, Lit),
    Neg(Lit),
    LT(Lit, Lit),
    And(Lit, Lit),
    Or(Lit, Lit),
}
impl Constraint {
    pub fn get_left(&self) -> &Lit {
        match self {
            Constraint::LEq(l1, _l2)
            | Constraint::Eq(l1, _l2)
            | Constraint::LT(l1, _l2)
            | Constraint::And(l1, _l2)
            | Constraint::Or(l1, _l2) => l1,
            Constraint::Neg(l) => l,
        }
    }

    pub fn get_right(&self) -> &Lit {
        match self {
            Constraint::LEq(_l1, l2)
            | Constraint::Eq(_l1, l2)
            | Constraint::LT(_l1, l2)
            | Constraint::And(_l1, l2)
            | Constraint::Or(_l1, l2) => l2,
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
            | Constraint::Or(l1, l2) => l1.get_variables().union(l2.get_variables()),
            Constraint::Neg(l) => l.get_variables(),
        }
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
                if sym_table.get_type(&p_i).unwrap() == &AtomType::Timepoint {
                    if sym_table.get_type(&p_j).unwrap() == &AtomType::Timepoint {
                        Ok(Relation::new(p_i, p_j, relation_type))
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
