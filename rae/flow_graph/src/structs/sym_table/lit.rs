use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::VarId;
use im::{hashset, HashSet};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    Exp(Vec<Lit>),
    Atom(VarId),
    Await(VarId),
    Read(Vec<VarId>),
    Write(Vec<VarId>),
    Exec(Vec<VarId>),
    Constraint(Box<Constraint>),
    Apply(Vec<VarId>),
}

impl Lit {
    pub fn apply(vec: Vec<VarId>) -> Self {
        Self::Apply(vec)
    }

    pub fn atom(atom: VarId) -> Self {
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

impl TryFrom<Lit> for VarId {
    type Error = LRuntimeError;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        value.borrow().try_into()
    }
}

impl TryFrom<&Lit> for VarId {
    type Error = LRuntimeError;

    fn try_from(value: &Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Atom(a) => Ok(*a),
            _ => Err(Default::default()),
        }
    }
}

impl TryFrom<&Lit> for Vec<VarId> {
    type Error = LRuntimeError;

    fn try_from(value: &Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Atom(a) | Lit::Await(a) => Ok(vec![*a]),
            Lit::Constraint(_) => Err(Default::default()),
            Lit::Exp(l) => {
                let mut e = vec![];
                for a in l {
                    e.push(VarId::try_from(a)?);
                }
                Ok(e)
            }
            Lit::Apply(vec) | Lit::Read(vec) | Lit::Write(vec) | Lit::Exec(vec) => Ok(vec.clone()),
        }
    }
}

impl TryFrom<Lit> for Vec<VarId> {
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

impl From<&VarId> for Lit {
    fn from(s: &VarId) -> Self {
        Self::Atom(*s)
    }
}

impl From<VarId> for Lit {
    fn from(s: VarId) -> Self {
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
        lv => Ok(match st.get_sym_id(&lv.to_string()) {
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
            Lit::Await(a) => {
                format!("await({})", a.format(st, sym_version))
            }
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

            Lit::Read(vec) => {
                let mut str = "read(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format(st, sym_version).as_str())
                }
                str.push(')');
                str
            }
            Lit::Write(vec) => {
                let mut str = "write(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format(st, sym_version).as_str())
                }
                str.push(')');
                str
            }
            Lit::Exec(vec) => {
                let mut str = "exec(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format(st, sym_version).as_str())
                }
                str.push(')');
                str
            }
        }
    }
}

impl FlatBindings for Lit {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Lit::Atom(a) | Lit::Await(a) => a.flat_bindings(st),
            Lit::Constraint(c) => c.flat_bindings(st),
            Lit::Exp(vec) => vec.flat_bindings(st),
            Lit::Apply(vec) | Lit::Read(vec) | Lit::Write(vec) | Lit::Exec(vec) => {
                vec.flat_bindings(st)
            }
        }
    }
}

impl GetVariables for Lit {
    fn get_variables(&self) -> HashSet<VarId> {
        match self {
            Lit::Atom(a) | Lit::Await(a) => hashset!(*a),
            Lit::Constraint(c) => c.get_variables(),
            Lit::Exp(vec) => {
                let mut hashset: im::HashSet<VarId> = Default::default();
                for e in vec {
                    hashset = hashset.union(e.get_variables())
                }
                hashset
            }
            Lit::Apply(vec) | Lit::Read(vec) | Lit::Write(vec) | Lit::Exec(vec) => {
                vec.iter().cloned().collect()
            }
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

impl Replace for Lit {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        match self {
            Lit::Atom(a) | Lit::Await(a) => a.replace(old, new),
            Lit::Constraint(c) => c.replace(old, new),
            Lit::Exp(e) => e.replace(old, new),
            Lit::Apply(vec) | Lit::Read(vec) | Lit::Write(vec) | Lit::Exec(vec) => {
                vec.replace(old, new)
            }
        }
    }
}
