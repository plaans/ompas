use crate::structs::lit::Lit;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};

#[derive(Clone)]
pub enum Constraint {
    LEq(Lit, Lit),
    Eq(Lit, Lit),
    Neg(Lit),
    LT(Lit, Lit),
    And(Lit, Lit),
    Or(Lit, Lit),
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
