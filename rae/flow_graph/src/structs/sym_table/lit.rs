use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use im::{hashset, HashSet};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum Lit {
    Exp(Vec<Lit>),
    Atom(AtomId),
    Await(AtomId),
    Constraint(Box<Constraint>),
    Apply(Vec<AtomId>),
}

impl Lit {
    pub fn apply(vec: Vec<AtomId>) -> Self {
        Self::Apply(vec)
    }

    pub fn atom(atom: AtomId) -> Self {
        Self::Atom(atom)
    }

    pub fn constraint(constraint: Constraint) -> Self {
        Self::Constraint(Box::new(constraint))
    }

    pub fn exp(exp: Vec<Lit>) -> Self {
        Self::Exp(exp)
    }

    pub fn is_atom(&self) -> bool {
        matches!(self, Self::Atom(_))
    }
    pub fn is_constraint(&self) -> bool {
        matches!(self, Self::Constraint(_))
    }
    pub fn is_exp(&self) -> bool {
        matches!(self, Self::Exp(_))
    }
}

impl TryFrom<Lit> for AtomId {
    type Error = LRuntimeError;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        value.borrow().try_into()
    }
}

impl TryFrom<&Lit> for AtomId {
    type Error = LRuntimeError;

    fn try_from(value: &Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Atom(a) => Ok(*a),
            _ => Err(Default::default()),
        }
    }
}

impl TryFrom<&Lit> for Vec<AtomId> {
    type Error = LRuntimeError;

    fn try_from(value: &Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Atom(a) => Ok(vec![*a]),
            Lit::Constraint(_) => Err(Default::default()),
            Lit::Exp(l) => {
                let mut e = vec![];
                for a in l {
                    e.push(AtomId::try_from(a)?);
                }
                Ok(e)
            }
            Lit::Apply(vec) => Ok(vec.clone()),
            Lit::Await(a) => Ok(vec![*a]),
        }
    }
}

impl TryFrom<Lit> for Vec<AtomId> {
    type Error = LRuntimeError;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        value.borrow().try_into()
    }
}

impl TryFrom<Lit> for Constraint {
    type Error = LRuntimeError;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Constraint(c) => Ok(c.deref().clone()),
            _ => Err(Default::default()),
        }
    }
}

impl TryFrom<&Lit> for Vec<Lit> {
    type Error = LRuntimeError;
    fn try_from(value: &Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Exp(c) => Ok(c.clone()),
            _ => Err(Default::default()),
        }
    }
}

impl TryFrom<Lit> for Vec<Lit> {
    type Error = LRuntimeError;
    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        value.borrow().try_into()
    }
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
        s.borrow().into()
    }
}

impl From<&Constraint> for Lit {
    fn from(c: &Constraint) -> Self {
        Self::Constraint(Box::new(c.clone()))
    }
}

impl From<Constraint> for Lit {
    fn from(c: Constraint) -> Self {
        c.borrow().into()
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

pub fn lvalue_to_lit(lv: &LValue, st: &mut RefSymTable) -> lruntimeerror::Result<Lit> {
    //println!("in lvalue_to_lit:\n{}", lv.format(0));
    //stdout().flush();
    match lv {
        LValue::List(list) => {
            let mut vec = vec![];
            for e in list.iter() {
                vec.push(lvalue_to_lit(e, st)?);
            }
            Ok(vec.into())
        }
        LValue::Map(_) => Err(lruntimeerror!(
            "LValue to lit",
            "Map transformation to lit is not supported yet."
        )),
        LValue::Number(n) => match n {
            LNumber::Int(i) => Ok(st.new_int(*i).into()),
            LNumber::Float(f) => Ok(st.new_float(*f).into()),
        },
        LValue::True => Ok(st.new_bool(true).into()),
        LValue::Nil => Ok(st.new_bool(false).into()),
        lv => Ok(match st.id(&lv.to_string()) {
            Some(id) => id.into(),
            None => {
                //println!("symbol {} does not exist", lv.to_string());
                st.new_symbol(&lv.to_string()).into()
            }
        }),
    }
}

impl FormatWithSymTable for Lit {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            Lit::Atom(a) => a.format(st, sym_version),
            Lit::Constraint(c) => c.format(st, sym_version),
            Lit::Exp(vec) => {
                let mut str = "(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format(st, sym_version).as_str())
                }
                str.push(')');
                str
            }
            Lit::Apply(vec) => {
                let mut str = "apply(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format(st, sym_version).as_str())
                }
                str.push(')');
                str
            }
            Lit::Await(a) => {
                format!("await({})", a.format(st, sym_version))
            }
        }
    }
}

impl FlatBindings for Lit {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Lit::Atom(a) => a.flat_bindings(st),
            Lit::Constraint(c) => c.flat_bindings(st),
            Lit::Exp(vec) => vec.flat_bindings(st),
            Lit::Apply(vec) => vec.flat_bindings(st),
            Lit::Await(a) => a.flat_bindings(st),
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
            Lit::Apply(vec) => vec.iter().cloned().collect(),
            Lit::Await(a) => hashset![*a],
        }
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| {
                sym_table.contained_in_domain(&sym_table.get_domain(v, true).unwrap(), domain)
            })
            .cloned()
            .collect()
    }
}

impl Replace for Lit {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        match self {
            Lit::Atom(a) => a.replace(old, new),
            Lit::Constraint(c) => c.replace(old, new),
            Lit::Exp(e) => e.replace(old, new),
            Lit::Apply(vec) => vec.replace(old, new),
            Lit::Await(a) => a.replace(old, new),
        }
    }
}
