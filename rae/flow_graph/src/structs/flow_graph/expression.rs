use crate::structs::chronicle::{FlatBindings, FormatWithSymTable};
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum Expression {
    Expr(AtomId),
    Handle(AtomId),
    Apply(Vec<AtomId>),
}

impl Expression {
    pub fn apply(vec: Vec<AtomId>) -> Self {
        Self::Apply(vec)
    }

    pub fn handle(handle: AtomId) -> Self {
        Self::Handle(handle)
    }

    pub fn expr(expr: AtomId) -> Self {
        Self::Expr(expr)
    }
}

impl FormatWithSymTable for Expression {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            Expression::Apply(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "apply({})", args)
            }
            Expression::Handle(h) => {
                write!(str, "handle({})", h.format(st, sym_version))
            }
            Expression::Expr(e) => write!(str, "{}", st.get_debug(e)),
        }
        .unwrap();
        str
    }
}

impl FlatBindings for Expression {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Expression::Apply(e) => e.flat_bindings(st),
            Expression::Expr(a) => a.flat_bindings(st),
            Expression::Handle(a) => a.flat_bindings(st),
        }
    }
}
