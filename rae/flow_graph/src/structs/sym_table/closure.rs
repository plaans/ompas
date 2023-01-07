use crate::structs::domain::root_type::{RootType, FALSE_ID, TRUE_ID};
use crate::structs::domain::Domain;
use crate::structs::sym_table::{AtomId, EmptyDomains, SymTable};
use std::rc::Rc;

pub(crate) type Proc = fn(&mut SymTable, &AtomId, Domain) -> EmptyDomains;

pub(crate) type ConstraintClosure =
    Rc<Box<dyn Fn(&mut SymTable, &AtomId, Domain, Proc) -> EmptyDomains>>;

pub(crate) type UpdateClosure = Rc<Box<dyn Fn(&mut SymTable, &AtomId) -> EmptyDomains>>;

pub(crate) fn union_constraint(vec: Vec<AtomId>) -> ConstraintClosure {
    Rc::new(Box::new(move |st, id: &AtomId, domain, proc| {
        let mut emptys = EmptyDomains::None;
        for d in &vec {
            emptys.append(proc(st, d, domain.clone()));
        }
        let emptys = emptys;

        emptys
    }))
}

pub(crate) fn union_update(vec: Vec<AtomId>) -> UpdateClosure {
    Rc::new(Box::new(move |st, id: &AtomId| {
        let mut emptys = EmptyDomains::None;
        let mut domain = Domain::empty();
        for d in &vec {
            let sub_domain = &st.domains[*d].domain;
            domain = st.union(&sub_domain, &domain);
        }
        let ancient_domain = &st.domains[*id].domain;
        let new_domain = st.meet(ancient_domain, &domain);
        st.domains[*id].domain = new_domain;

        if st.domains[*id].domain.is_empty() {
            emptys.append(EmptyDomains::Some(vec![*id]))
        }
        emptys
    }))
}

pub(crate) fn composed_constraint(atom: AtomId) -> ConstraintClosure {
    Rc::new(Box::new(move |st, id: &AtomId, domain, proc| {
        let mut emptys = EmptyDomains::None;
        let d = st.get_domain(id, false).unwrap().clone();
        if let Domain::Composed(t1, _) = d {
            if let Domain::Composed(t2, subtypes) = domain {
                if t1 == t2 {
                    emptys.append(proc(st, &atom, subtypes[0].clone()));
                }
            }
        }

        emptys
    }))
}

pub(crate) fn composed_update(atom: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st, id: &AtomId| {
        let atom = st.get_parent(&atom);
        let sub = st.domains[atom].domain.clone();

        if sub.is_empty() {
            st.domains[*id].domain = Domain::empty();
            return EmptyDomains::Some(vec![*id]);
        }
        let d = match st.domains[*id].domain.clone() {
            Domain::Simple(t) => Domain::composed(t, vec![sub]),
            Domain::Composed(t, _) => Domain::composed(t, vec![sub]),
            _ => unreachable!(),
        };

        st.domains[*id].domain = d;
        EmptyDomains::None
    }))
}

pub(crate) fn result_is_err_update(arg_err: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st, id: &AtomId| {
        let arg_err = st.get_parent(&arg_err);
        let d = st.get_domain(&arg_err, false).unwrap().clone();

        if d.is_empty() {
            EmptyDomains::Some(vec![*id])
        } else {
            let d = if st.contained_in_domain(&d, &RootType::Err.into()) {
                st.meet(&st.domains[*id].domain, &RootType::True.into())
            } else if st.meet(&d, &RootType::Err.into()).is_empty() {
                st.meet(&st.domains[*id].domain, &RootType::False.into())
            } else {
                st.domains[*id].domain.clone()
            };
            let r = if d.is_empty() {
                EmptyDomains::Some(vec![*id])
            } else {
                EmptyDomains::None
            };

            st.domains[*id].domain = d;
            r
        }
    }))
}

pub(crate) fn arg_is_err_update(result_err: AtomId) -> UpdateClosure {
    Rc::new(Box::new(move |st, id: &AtomId| {
        let result_err = st.get_parent(&result_err);
        let d = st.get_domain(&result_err, false).unwrap().clone();
        let d = if d.is_true() {
            st.meet(&st.domains[*id].domain, &RootType::Err.into())
        } else if d.is_false() {
            st.substract(&st.domains[*id].domain, &RootType::Err.into())
        } else {
            st.domains[*id].domain.clone()
        };

        let r = if d.is_empty() {
            EmptyDomains::Some(vec![*id])
        } else {
            EmptyDomains::None
        };

        st.domains[*id].domain = d;
        r
    }))
}

pub(crate) fn meet_to_domain(st: &mut SymTable, id: &AtomId, domain: Domain) -> EmptyDomains {
    st.meet_to_domain(id, domain)
}

pub(crate) fn substract_to_domain(st: &mut SymTable, id: &AtomId, domain: Domain) -> EmptyDomains {
    st.subtract_to_domain(id, domain)
}

pub(crate) fn set_domain(st: &mut SymTable, id: &AtomId, domain: Domain) -> EmptyDomains {
    st.set_domain(id, domain)
}
