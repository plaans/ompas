use crate::point_algebra::problem::{try_into_pa_relation, PAGraph, PAProblem};
use crate::point_algebra::remove_useless_timepoints;
use crate::structs::chronicle::chronicle::{ChronicleSet, ChronicleTemplate};
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, GetVariables};
use im::HashSet;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::borrow::Borrow;

pub fn post_processing(c: &mut ChronicleTemplate) -> Result<(), LRuntimeError> {
    //add_constraint_on_end_timepoint(c, context, ch);
    c.sym_table.flat_bindings();
    unify_equal(c);
    //ch.sym_table.flat_bindings();
    //panic!("for no fucking reason");
    /*println!(
        "before timepoint simplification: {}",
        c.format(&ch.sym_table, true)
    );*/
    //simplify_timepoints(c)?;
    rm_useless_var(c);
    /*println!(
        "before merge conditions: {}",
        c.format_with_sym_table(&ch.sym_table, true)
    );*/
    //merge_conditions(c, context, ch)?;
    //simplify_constraints(c, context, ch)?;
    //c.format_with_parent(&ch.get_mut_sym_table());

    Ok(())
}

pub fn unify_equal(c: &mut ChronicleTemplate) {
    //println!("before binding: {}", c.format(true));

    let mut vec_constraint_to_rm = vec![];
    let constraints = c.get_constraints().clone();

    for (index, constraint) in constraints.iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                if let Ok(true) = bind_atoms(id_1, id_2, &mut c.sym_table) {
                    vec_constraint_to_rm.push(index);
                }
            }
        }
    }

    //println!("after binding: {}", c.format(true));

    /*println!(
        "constraints to rm after binding: {:?}",
        vec_constraint_to_rm
    );*/

    c.rm_set_constraint(vec_constraint_to_rm);
    c.sym_table.flat_bindings();
}

/// Returns true if the constraint can be safely deleted
fn bind_atoms(id_1: &AtomId, id_2: &AtomId, st: &mut RefSymTable) -> Result<bool, LRuntimeError> {
    let id_1 = st.get_parent(id_1);
    let id_2 = st.get_parent(id_2);
    let id_1 = &id_1;
    let id_2 = &id_2;

    let (id_1, id_2) = if id_1 < id_2 {
        (id_1, id_2)
    } else {
        (id_2, id_1)
    };

    let type_1 = st.get_type_of(id_1);
    let type_2 = st.get_type_of(id_2);

    let atom_1 = st.get_atom(id_1, false).unwrap().clone();
    let atom_2 = st.get_atom(id_2, false).unwrap().clone();

    let constant_1 = atom_1.is_constant();
    let constant_2 = atom_2.is_constant();

    match (constant_1, constant_2) {
        (true, true) => {
            if type_1 != type_2 {
                return Err(Default::default());
            }
            if st.get_atom(id_1, false).unwrap() != st.get_atom(id_2, false).unwrap() {
                return Err(Default::default());
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
        (true, false) => {
            match &type_2 {
                AtomType::Untyped => st.set_type_of(id_2, &type_1),
                t => {
                    if t != &type_1 {
                        return Err(Default::default());
                    }
                }
            }

            return if atom_2.is_parameter() {
                Ok(false)
            } else {
                st.union_atom(id_1, id_2);
                Ok(true)
            };
        }
        (false, true) => {
            match &type_1 {
                AtomType::Untyped => st.set_type_of(id_1, &type_2),
                t => {
                    if t != &type_1 {
                        return Err(Default::default());
                    }
                }
            }

            if atom_1.is_parameter() {
                Ok(false)
            } else {
                st.union_atom(id_2, id_1);
                Ok(true)
            }
        }
        (false, false) => {
            let parameter_1 = atom_1.is_parameter();
            let parameter_2 = atom_2.is_parameter();

            match (type_1, type_2) {
                (AtomType::Untyped, AtomType::Untyped) => {
                    //
                }
                (AtomType::Untyped, t) => st.set_type_of(id_1, &t),
                (t, AtomType::Untyped) => st.set_type_of(id_2, &t),
                (t1, t2) => {
                    if t1 != t2 {
                        return Err(Default::default());
                    }
                }
            }

            let (parent, child) = match (parameter_1, parameter_2) {
                (true, true) => (id_1, id_2),
                (true, false) => (id_1, id_2),
                (false, true) => (id_2, id_1),
                (false, false) => (id_1, id_2),
            };

            st.union_atom(parent, child);
            Ok(true)
        }
    }
}

pub fn rm_useless_var(c: &mut ChronicleTemplate) {
    //Variables in expressions
    c.format_with_parent();
    let parameters: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .filter_map(|v| {
            if c.sym_table.get_atom(v, false).unwrap().is_parameter() {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    let used_vars: HashSet<AtomId> = c
        .get_all_variables_in_sets()
        .iter()
        .filter_map(|v| {
            if c.sym_table.get_atom(v, false).unwrap().is_variable() {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    c.variables.clear();

    let new_vars = used_vars.union(parameters);
    for v in &new_vars {
        assert_eq!(*v, c.sym_table.get_parent(v));
    }
    for v in new_vars {
        c.add_var(&v)
    }
}

pub fn simplify_timepoints(c: &mut ChronicleTemplate) -> Result<(), LRuntimeError> {
    /*let format_hash = |h: &HashSet<AtomId>| -> String {
        let mut string = "{".to_string();
        for e in h {
            string.push_str(e.to_string().as_str());
            string.push(' ');
        }
        string.push('}');
        string
    };*/

    let timepoints: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .map(|a| c.sym_table.get_parent(a))
        .filter(|a| c.sym_table.get_type_of(a) == AtomType::Timepoint)
        .collect();

    //println!("timepoints: {}", format_hash(&timepoints));

    let used_timepoints: HashSet<AtomId> = c
        .get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
        .iter()
        .map(|a| c.sym_table.get_parent(a))
        .filter(|a| c.sym_table.get_type_of(a) == AtomType::Timepoint)
        .collect();

    // println!("used timepoints: {}", format_hash(&used_timepoints));

    let hard_timepoints: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .map(|a| c.sym_table.get_parent(a))
        .filter(|a| {
            let is_variable = c.sym_table.get_atom(a, false).unwrap().is_parameter();
            let t = c.sym_table.get_type_of(a);
            is_variable && t == AtomType::Timepoint
        })
        .collect();

    //println!("hard timepoints: {}", format_hash(&hard_timepoints));

    let used_timepoints = used_timepoints.union(hard_timepoints);

    let optional_timepoints: HashSet<AtomId> = timepoints.clone().difference(used_timepoints);
    //println!("optional_timepoints: {}", format_hash(&optional_timepoints));
    let mut relations = vec![];
    let mut index_temporal_constraints = vec![];
    for (i, constraint) in c.get_constraints().iter().enumerate() {
        if matches!(constraint, Constraint::Not(_)) {
        } else if let Ok(r) = try_into_pa_relation(constraint, &c.sym_table) {
            index_temporal_constraints.push(i);
            relations.push(r);
        }
    }

    c.rm_set_constraint(index_temporal_constraints);

    let mut timepoints: Vec<AtomId> = timepoints.iter().cloned().collect();
    timepoints.sort();
    let timepoints = timepoints
        .iter()
        .map(|a| (*a, optional_timepoints.contains(a)))
        .collect();
    //println!("st: {}", ch.get_mut_sym_table());

    let problem: PAProblem<AtomId> = PAProblem::new(timepoints, relations);

    //println!("problem: {:?}", problem);

    let graph: PAGraph<AtomId> = problem.borrow().into();
    //graph.print();
    let new_graph = remove_useless_timepoints(graph)?;
    //new_graph.print();

    let problem: PAProblem<AtomId> = new_graph.into();
    for r in problem.get_relations() {
        c.add_constraint(r.into())
    }
    Ok(())
}

/*pub fn simplify_constraints(c: &mut ChronicleTemplate) -> Result<(), LRuntimeError> {
    //simple case where
    let mut vec: Vec<(usize, Constraint)> = vec![];
    for (i, c) in c.pc.constraints.iter().enumerate() {
        if let Constraint::Eq(a, b) = c {
            match (a, b) {
                (Lit::Atom(a), Lit::Constraint(b)) => {
                    if let Atom::Bool(true) = c.sym_table.get_atom(a, true).unwrap() {
                        /*println!(
                            "{} => {}",
                            c.format(&ch.sym_table, true),
                            b.format(&ch.sym_table, true)
                        );*/
                        vec.push((i, b.deref().clone()));
                    }
                }
                (Lit::Constraint(b), Lit::Atom(a)) => {
                    if let Atom::Bool(true) = c.sym_table.get_atom(a, true).unwrap() {
                        /*println!(
                            "{} => {}",
                            c.clone().format(&ch.sym_table, true),
                            b.format(&ch.sym_table, true)
                        );*/
                        vec.push((i, b.deref().clone()));
                    }
                }
                _ => {}
            }
        }
    }
    vec.drain(..)
        .for_each(|(i, cons)| c.pc.constraints[i] = cons);

    Ok(())
}

pub fn merge_conditions(
    c: &mut ChronicleTemplate,
    _: &ConversionContext,
    ch: &mut ConversionCollection,
) -> Result<(), LRuntimeError> {
    let mut c_to_remove: HashSet<usize> = Default::default();

    for (i, c1) in c.get_conditions().iter().enumerate() {
        let next = i + 1;
        for (j, c2) in c.get_conditions()[next..].iter().enumerate() {
            let index = j + next;
            if c1.interval == c2.interval && c1.sv == c2.sv {
                /*println!(
                    "merging {}({}) and {}({})",
                    c1.format(&ch.sym_table, true),
                    i,
                    c2.format(&ch.sym_table, true),
                    index
                );*/
                bind_atoms(&c1.value, &c2.value, &mut ch.get_mut_sym_table())?;
                c_to_remove.insert(index);
            }
        }
    }

    let mut vec: Vec<usize> = c_to_remove.iter().copied().collect();

    //println!("condition to remove: {:?}", vec);
    vec.sort_unstable();
    //println!("condition to remove(sorted): {:?}", vec);
    vec.reverse();
    //println!("condition to remove(reversed): {:?}", vec);
    vec.iter().for_each(|i| c.rm_condition(*i));

    ch.sym_table.flat_bindings();
    Ok(())
}

pub fn add_constraint_on_end_timepoint(
    c: &mut ChronicleTemplate,
    _: &ConversionContext,
    ch: &mut ConversionCollection,
) {
    let timepoints: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .map(|a| *ch.sym_table.get_parent(a))
        .filter(|a| ch.sym_table.get_type_of(a).unwrap() == Some(AtomType::Timepoint))
        .collect();
    let end = *c.get_end();
    for t in &timepoints {
        c.add_constraint(Constraint::leq(t, end));
    }
}*/
