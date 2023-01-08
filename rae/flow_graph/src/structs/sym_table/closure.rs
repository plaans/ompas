use crate::structs::domain::root_type::{RootType, FALSE_ID, TRUE_ID};
use crate::structs::domain::Domain;
use crate::structs::sym_table::{AtomId, EmptyDomains, SymTable};
use std::rc::Rc;

pub(crate) type Proc = fn(&mut SymTable, &AtomId, Domain) -> EmptyDomains;

/*pub(crate) type ConstraintClosure =
Rc<Box<dyn Fn(&mut SymTable, &AtomId, Domain, Proc) -> EmptyDomains>>;*/

pub(crate) type UpdateClosure = Rc<Box<dyn Fn(&mut SymTable) -> EmptyDomains>>;

/*pub(crate) fn union_constraint(vec: Vec<AtomId>) -> ConstraintClosure {
    Rc::new(Box::new(move |st, id: &AtomId, domain, proc| {
        let mut emptys = EmptyDomains::None;
        for d in &vec {
            emptys.append(proc(st, d, domain.clone()));
        }
        let emptys = emptys;

        emptys
    }))
}*/

#[derive(Clone)]
pub struct Update {
    pub closure: UpdateClosure,
    pub id: AtomId,
}

impl Update {
    pub fn new(id: AtomId, closure: UpdateClosure) -> Self {
        Self { closure, id }
    }
}

pub(crate) fn in_union_update(id: AtomId, union_atom: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let union_atom = st.get_parent(&union_atom);
        let union_domain = &st.domains[union_atom].domain;
        let in_union_domain = &st.domains[id].domain;
        let domain = st.meet(union_domain, in_union_domain);
        st.domains[id].domain = domain;
        if st.domains[id].domain.is_empty() {
            EmptyDomains::Some(vec![id])
        } else {
            EmptyDomains::None
        }
    }))
}

pub(crate) fn union_update(id: AtomId, mut union: Vec<AtomId>) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let union: Vec<AtomId> = union.iter().map(|id| st.get_parent(id)).collect();

        let mut emptys = EmptyDomains::None;
        let mut new_domain = Domain::empty();
        for d in &union {
            let sub_domain = &st.domains[*d].domain;
            new_domain = st.union(&sub_domain, &new_domain);
        }
        let ancient_domain = &st.domains[id].domain;
        let new_domain = st.meet(ancient_domain, &new_domain);
        st.domains[id].domain = new_domain;

        if st.domains[id].domain.is_empty() {
            emptys.append(EmptyDomains::Some(vec![id]))
        }
        emptys
    }))
}

pub(crate) fn in_composed_update(id: AtomId, composed: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let composed = st.get_parent(&composed);
        let composed = st.domains[composed].domain.clone();

        if composed.is_empty() {
            st.domains[id].domain = Domain::empty();
            EmptyDomains::Some(vec![id])
        } else if let Domain::Composed(t, s) = composed {
            let sub = s[0].clone();
            let ancient_domain = &st.domains[id].domain;
            let new_domain = st.meet(&sub, ancient_domain);
            let r = if new_domain.is_empty() {
                st.domains[id].domain = Domain::empty();
                EmptyDomains::Some(vec![id])
            } else {
                EmptyDomains::None
            };
            st.domains[id].domain = new_domain;

            r
        } else {
            unreachable!()
        }
    }))
}

pub(crate) fn composed_update(id: AtomId, atom: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let atom = st.get_parent(&atom);
        let sub = st.domains[atom].domain.clone();

        let ancient_domain = &st.domains[id].domain;

        let d = match ancient_domain {
            Domain::Simple(t) => Domain::composed(*t, vec![sub]),
            Domain::Composed(t, _) => Domain::composed(*t, vec![sub]),
            _ => unreachable!(),
        };
        let new_domain = st.meet(&d, &ancient_domain);

        let r = if new_domain.is_empty() {
            EmptyDomains::Some(vec![id])
        } else {
            EmptyDomains::None
        };

        st.domains[id].domain = new_domain;
        r
    }))
}

pub(crate) fn result_is_err_update(id: AtomId, arg_err: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let arg_err = st.get_parent(&arg_err);
        let d = st.get_domain(&arg_err, false).unwrap().clone();

        if d.is_empty() {
            EmptyDomains::Some(vec![id])
        } else {
            let d = if st.contained_in_domain(&d, &RootType::Err.into()) {
                st.meet(&st.domains[id].domain, &RootType::True.into())
            } else if st.meet(&d, &RootType::Err.into()).is_empty() {
                st.meet(&st.domains[id].domain, &RootType::False.into())
            } else {
                st.domains[id].domain.clone()
            };
            let r = if d.is_empty() {
                EmptyDomains::Some(vec![id])
            } else {
                EmptyDomains::None
            };

            st.domains[id].domain = d;
            r
        }
    }))
}

pub(crate) fn arg_is_err_update(id: AtomId, result_err: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let id = st.get_parent(&id);
        let result_err = st.get_parent(&result_err);
        let d = st.get_domain(&result_err, false).unwrap().clone();
        let d = if d.is_true() {
            st.meet(&st.domains[id].domain, &RootType::Err.into())
        } else if d.is_false() {
            st.substract(&st.domains[id].domain, &RootType::Err.into())
        } else {
            st.domains[id].domain.clone()
        };

        let r = if d.is_empty() {
            EmptyDomains::Some(vec![id])
        } else {
            EmptyDomains::None
        };

        st.domains[id].domain = d;
        r
    }))
}
