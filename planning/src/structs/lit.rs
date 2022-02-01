use crate::structs::constraint::Constraint;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};
use im::{hashset, HashSet};
use ompas_lisp::core::structs::lerror;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use ompas_lisp::core::structs::lvalue::LValue;

#[derive(Clone)]
pub enum Lit {
    Atom(AtomId),
    Constraint(Box<Constraint>),
    Exp(Vec<Lit>),
}

impl Default for Lit {
    fn default() -> Self {
        Self::Exp(vec![])
    }
}

impl From<&AtomId> for Lit {
    fn from(s: &AtomId) -> Self {
        Self::Atom(*s)
    }
}

impl From<AtomId> for Lit {
    fn from(s: AtomId) -> Self {
        (&s).into()
    }
}

impl From<&Constraint> for Lit {
    fn from(c: &Constraint) -> Self {
        Self::Constraint(Box::new(c.clone()))
    }
}

impl From<Constraint> for Lit {
    fn from(c: Constraint) -> Self {
        (&c).into()
    }
}

impl<T: Clone + Into<Lit>> From<&Vec<T>> for Lit {
    fn from(v: &Vec<T>) -> Self {
        Lit::Exp(v.iter().map(|e| e.clone().into()).collect())
    }
}

impl<T: Clone + Into<Lit>> From<Vec<T>> for Lit {
    fn from(v: Vec<T>) -> Self {
        (&v).into()
    }
}

pub fn lvalue_to_lit(lv: &LValue, st: &mut SymTable) -> lerror::Result<Lit> {
    match lv {
        LValue::List(list) => {
            let mut vec = vec![];
            for e in list {
                vec.push(lvalue_to_lit(e, st)?);
            }
            Ok(vec.into())
        }
        LValue::Map(_) => Err(SpecialError(
            "LValue to lit",
            "Map transformation to lit is not supported yet.".to_string(),
        )),
        LValue::Number(n) => Ok(st.new_number(n.clone()).into()),
        LValue::True => Ok(st.new_bool(true).into()),
        LValue::Nil => Ok(st.new_bool(false).into()),
        lv => Ok(st.declare_new_symbol(lv.to_string(), false).into()),
    }
}

impl FormatWithSymTable for Lit {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Lit::Atom(a) => st.get_sym(a).to_string(),
            Lit::Constraint(c) => c.format_with_sym_table(st),
            Lit::Exp(vec) => {
                let mut str = "(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format_with_sym_table(st).as_str())
                }
                str.push(')');
                str
            }
        }
    }
}

impl GetVariables for Lit {
    fn get_variables(&self) -> HashSet<AtomId> {
        match self {
            Lit::Atom(a) => hashset!(*a),
            Lit::Constraint(c) => c.get_variables(),
            Lit::Exp(vec) => {
                let mut hashset: im::HashSet<AtomId> = Default::default();
                for e in vec {
                    hashset = hashset.union(e.get_variables())
                }
                hashset
            }
        }
    }
}
