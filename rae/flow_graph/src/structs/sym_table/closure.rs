use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::{False, True};
use crate::structs::domain::Domain;
use crate::structs::sym_table::{AtomId, EmptyDomains, SymTable};
use std::rc::Rc;

pub(crate) type UpdateClosure = Rc<Box<dyn Fn(&mut SymTable) -> EmptyDomains>>;

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

pub(crate) fn union_update(id: AtomId, union: Vec<AtomId>) -> UpdateClosure {
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
        } else if let Domain::Composed(_, s) = composed {
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

pub(crate) fn result_branch_cond_update(
    cond: AtomId,
    result_branch: AtomId,
    branch: bool,
) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let cond = st.get_parent(&cond);
        let result_branch = st.get_parent(&result_branch);

        let mut empty = EmptyDomains::None;

        if st.domains[result_branch].domain.is_empty() {
            let branch = match branch {
                true => False.into(),
                false => True.into(),
            };
            let domain = st.meet(&branch, &st.domains[cond].domain);
            if domain.is_empty() {
                empty.append(EmptyDomains::Some(vec![cond]))
            }
            st.domains[cond].domain = domain
        }

        empty
    }))
}

pub(crate) fn cond_result_branching_update(
    cond: AtomId,
    result_branching: AtomId,
    true_result: AtomId,
    false_result: AtomId,
) -> UpdateClosure {
    Rc::new(Box::new(move |st| {
        let cond = st.get_parent(&cond);
        let true_result = st.get_parent(&true_result);
        let false_result = st.get_parent(&false_result);
        let result_branching = st.get_parent(&result_branching);

        let mut empty = EmptyDomains::None;

        if st.domains[cond].domain.is_true() {
            st.remove_update(&false_result, &result_branching);
            st.remove_update(&result_branching, &false_result);
            empty.append(st.union_atom(&true_result, &result_branching));
        } else if st.domains[cond].domain.is_false() {
            st.remove_update(&true_result, &result_branching);
            st.remove_update(&result_branching, &true_result);
            empty.append(st.union_atom(&false_result, &result_branching));
        }

        empty
    }))
}
