use crate::planning::point_algebra::problem::{Graph, Problem};
use crate::planning::point_algebra::remove_useless_timepoints;
use crate::planning::structs::atom::Atom;
use crate::planning::structs::chronicle::{ChronicleSet, ChronicleTemplate};
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::{AtomKind, PlanningAtomType, VariableKind};
use crate::planning::structs::{ConversionCollection, ConversionContext};
use im::HashSet;
use ompas_lisp::core::structs::lerror::LError;
use std::ops::Deref;

pub fn post_processing(
    c: &mut ChronicleTemplate,
    context: &ConversionContext,
    ch: &mut ConversionCollection,
) -> Result<(), LError> {
    //add_constraint_on_end_timepoint(c, context, ch);
    unify_equal(c, ch, context);
    ch.sym_table.flat_bindings();
    //panic!("for no fucking reason");
    simplify_timepoints(c, ch, context)?;
    rm_useless_var(c, ch, context);
    /*println!(
        "before merge conditions: {}",
        c.format_with_sym_table(&ch.sym_table, true)
    );*/
    merge_conditions(c, context, ch)?;
    simplify_constraints(c, context, ch)?;
    c.format_with_parent(&ch.sym_table);

    Ok(())
}

pub fn simplify_constraints(
    c: &mut ChronicleTemplate,
    _: &ConversionContext,
    ch: &mut ConversionCollection,
) -> Result<(), LError> {
    //simple case where
    let mut vec: Vec<(usize, Constraint)> = vec![];
    for (i, c) in c.pc.constraints.iter().enumerate() {
        if let Constraint::Eq(a, b) = c {
            match (a, b) {
                (Lit::Atom(a), Lit::Constraint(b)) => {
                    if let Atom::Bool(true) = ch.sym_table.get_atom(a, true).unwrap() {
                        println!(
                            "{} => {}",
                            c.format(&ch.sym_table, true),
                            b.format(&ch.sym_table, true)
                        );
                        vec.push((i, b.deref().clone()));
                    }
                }
                (Lit::Constraint(b), Lit::Atom(a)) => {
                    if let Atom::Bool(true) = ch.sym_table.get_atom(a, true).unwrap() {
                        println!(
                            "{} => {}",
                            c.clone().format(&ch.sym_table, true),
                            b.format(&ch.sym_table, true)
                        );
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
) -> Result<(), LError> {
    let mut c_to_remove: HashSet<usize> = Default::default();

    for (i, c1) in c.get_conditions().iter().enumerate() {
        let next = i + 1;
        for (j, c2) in c.get_conditions()[next..].iter().enumerate() {
            let index = j + next;
            if c1.interval == c2.interval && c1.sv == c2.sv {
                println!(
                    "merging {}({}) and {}({})",
                    c1.format(&ch.sym_table, true),
                    i,
                    c2.format(&ch.sym_table, true),
                    index
                );
                bind_atoms(&c1.value, &c2.value, &mut ch.sym_table)?;
                c_to_remove.insert(index);
            }
        }
    }

    let mut vec: Vec<usize> = c_to_remove.iter().copied().collect();

    println!("condition to remove: {:?}", vec);
    vec.sort_unstable();
    println!("condition to remove(sorted): {:?}", vec);
    vec.reverse();
    println!("condition to remove(reversed): {:?}", vec);
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
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Timepoint)
        })
        .collect();
    let end = *c.get_end();
    for t in &timepoints {
        c.add_constraint(Constraint::leq(t, end));
    }
}

/// Returns true if the constraint can be safely deleted
fn bind_atoms(id_1: &AtomId, id_2: &AtomId, st: &mut SymTable) -> Result<bool, LError> {
    let id_1 = *st.get_parent(id_1);
    let id_2 = *st.get_parent(id_2);
    let id_1 = &id_1;
    let id_2 = &id_2;
    let type_1 = *st.get_type_of(id_1).expect("id should be defined");
    let type_2 = *st.get_type_of(id_2).expect("id should be defined");

    match (type_1.kind, type_2.kind) {
        (AtomKind::Constant, AtomKind::Constant) => {
            if type_1.a_type != type_2.a_type {
                return Err(Default::default());
            }
            if st.get_atom(id_1, false).unwrap() != st.get_atom(id_2, false).unwrap() {
                return Err(Default::default());
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
        (AtomKind::Constant, AtomKind::Variable(kind)) => {
            match &type_2.a_type {
                Some(t) => {
                    if t != &type_2.a_type.unwrap() {
                        return Err(Default::default());
                    }
                }
                None => st.set_type_of(id_2, &type_2.a_type),
            }

            match kind {
                VariableKind::Local => {
                    st.union_atom(id_1, id_2);
                    Ok(true)
                }
                VariableKind::Parameter => Ok(false),
            }
        }
        (AtomKind::Variable(kind), AtomKind::Constant) => {
            match &type_1.a_type {
                Some(t) => {
                    if t != &type_1.a_type.unwrap() {
                        return Err(Default::default());
                    }
                }
                None => st.set_type_of(id_1, &type_2.a_type),
            }

            match kind {
                VariableKind::Local => {
                    st.union_atom(id_2, id_1);
                    Ok(true)
                }
                VariableKind::Parameter => Ok(false),
            }
        }
        (
            AtomKind::Variable(VariableKind::Parameter),
            AtomKind::Variable(VariableKind::Parameter),
        ) => {
            let ((id_1, type_1), (id_2, type_2)) = if id_1 < id_2 {
                ((id_1, type_1), (id_2, type_2))
            } else {
                ((id_2, type_2), (id_1, type_1))
            };

            match (type_1.a_type.is_some(), type_2.a_type.is_some()) {
                (true, true) => {
                    if type_1.a_type.unwrap() == type_2.a_type.unwrap() {
                        st.union_atom(id_1, id_2);
                    } else {
                        return Err(Default::default());
                    }
                }
                (true, false) => st.union_atom(id_1, id_2),
                (false, true) => st.union_atom(id_2, id_1),
                (false, false) => st.union_atom(id_1, id_2),
            }
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Local), AtomKind::Variable(VariableKind::Local)) => {
            /*let ((id_1, type_1), (id_2, type_2)) = if id_1 < id_2 {
                ((id_1, type_1), (id_2, type_2))
            } else {
                ((id_2, type_2), (id_1, type_1))
            };*/
            match (type_1.a_type.is_some(), type_2.a_type.is_some()) {
                (true, true) => {
                    if type_1.a_type.unwrap() == type_2.a_type.unwrap() {
                        st.union_atom(id_1, id_2);
                    } else {
                        return Err(Default::default());
                    }
                }
                (true, false) => st.union_atom(id_1, id_2),
                (false, true) => st.union_atom(id_2, id_1),
                (false, false) => st.union_atom(id_1, id_2),
            }
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Local), AtomKind::Variable(VariableKind::Parameter)) => {
            if type_1.a_type.is_some() && type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            } else if type_1.a_type.is_some() {
                /*println!(
                    "new type of {}({}) -> {}",
                    id_2.format_with_sym_table(st, true),
                    type_2.a_type.format_with_sym_table(st, true),
                    type_1.a_type.format_with_sym_table(st, true)
                );*/
                st.set_type_of(id_2, &type_1.a_type)
            }
            st.union_atom(id_2, id_1);
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Parameter), AtomKind::Variable(VariableKind::Local)) => {
            if type_1.a_type.is_some() && type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            } else if type_2.a_type.is_some() {
                /*println!(
                    "new type of {}({}) -> {}",
                    id_1.format_with_sym_table(st, true),
                    type_1.a_type.format_with_sym_table(st, true),
                    type_2.a_type.format_with_sym_table(st, true)
                );*/
                st.set_type_of(id_1, &type_2.a_type)
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
    }
}

pub fn unify_equal(
    c: &mut ChronicleTemplate,
    ch: &mut ConversionCollection,
    _context: &ConversionContext,
) {
    let mut vec_constraint_to_rm = vec![];
    for (index, constraint) in c.get_constraints().iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                if let Ok(true) = bind_atoms(id_1, id_2, &mut ch.sym_table) {
                    vec_constraint_to_rm.push(index);
                }
            }
        }
    }

    c.rm_set_constraint(vec_constraint_to_rm);
    ch.sym_table.flat_bindings();
}

pub fn rm_useless_var(
    c: &mut ChronicleTemplate,
    ch: &mut ConversionCollection,
    _context: &ConversionContext,
) {
    //Variables in expressions
    c.format_with_parent(&ch.sym_table);
    let parameters: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .filter_map(|v| {
            if ch.sym_table.get_type_of(v).unwrap().kind
                == AtomKind::Variable(VariableKind::Parameter)
            {
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
            if let AtomKind::Variable(_) = ch.sym_table.get_type_of(v).unwrap().kind {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    c.pc.variables.clear();

    let new_vars = used_vars.union(parameters);
    for v in &new_vars {
        assert_eq!(v, ch.sym_table.get_parent(v));
    }
    c.add_variables(new_vars);
}

pub fn simplify_timepoints(
    c: &mut ChronicleTemplate,
    ch: &mut ConversionCollection,
    _: &ConversionContext,
) -> Result<(), LError> {
    let timepoints: HashSet<AtomId> = c
        .get_variables()
        .iter()
        .map(|a| *ch.sym_table.get_parent(a))
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Timepoint)
        })
        .collect();
    let used_timepoints: HashSet<AtomId> = c
        .get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
        .iter()
        .map(|a| *ch.sym_table.get_parent(a))
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Timepoint)
        })
        .collect();

    let optional_timepoints: HashSet<AtomId> = timepoints.clone().difference(used_timepoints);
    let mut relations = vec![];
    let mut index_temporal_constraints = vec![];
    for (i, constraint) in c.get_constraints().iter().enumerate() {
        if matches!(constraint, Constraint::Not(_)) {
        } else if let Ok(r) = constraint.try_into_pa_relation(&ch.sym_table) {
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
    let problem: Problem<AtomId> = Problem::new(timepoints, relations);

    let graph: Graph<AtomId> = (&problem).into();
    let new_graph = remove_useless_timepoints(graph)?;
    let problem: Problem<AtomId> = new_graph.into();
    for r in problem.get_relations() {
        c.add_constraint(r.into())
    }
    Ok(())
}
