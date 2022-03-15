use crate::planning::point_algebra::problem::{Graph, Problem};
use crate::planning::point_algebra::remove_useless_timepoints;
use crate::planning::structs::chronicle::{Chronicle, ChronicleSet};
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::{AtomKind, PlanningAtomType, VariableKind};
use crate::planning::structs::{ChronicleHierarchy, ConversionContext};
use im::{hashset, HashSet};
use ompas_lisp::core::structs::lerror::LError;

pub fn post_processing(
    c: &mut Chronicle,
    context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<(), LError> {
    unify_equal(c, ch, context);
    ch.sym_table.flat_bindings();
    //panic!("for no fucking reason");
    simplify_timepoints(c, ch, context)?;
    rm_useless_var(c, ch, context);
    Ok(())
}

/// Returns true if the constraint can be safely deleted
pub fn bind_variables(id_1: &AtomId, id_2: &AtomId, st: &mut SymTable) -> Result<bool, LError> {
    let id_1 = *st.get_parent(id_1);
    let id_2 = *st.get_parent(id_2);
    let id_1 = &id_1;
    let id_2 = &id_2;
    let type_1 = *st.get_type_of(id_1).expect("id should be defined");
    let type_2 = *st.get_type_of(id_2).expect("id should be defined");
    /*println!(
        "binding: {}({}), {}({})",
        id_1.format_with_sym_table(st),
        type_1.format_with_sym_table(st),
        id_2.format_with_sym_table(st),
        type_2.format_with_sym_table(st)
    );*/

    match (type_1.kind, type_2.kind) {
        (AtomKind::Constant, AtomKind::Constant) => {
            if type_1.a_type != type_2.a_type {
                return Err(Default::default());
            }
            if st.get_atom(id_1).unwrap() != st.get_atom(id_2).unwrap() {
                return Err(Default::default());
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
        (AtomKind::Constant, AtomKind::Variable(_)) => {
            if type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
        (AtomKind::Variable(_), AtomKind::Constant) => {
            if type_1.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            }
            st.union_atom(id_2, id_1);
            Ok(true)
        }
        (
            AtomKind::Variable(VariableKind::Parameter),
            AtomKind::Variable(VariableKind::Parameter),
        ) => {
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
                println!(
                    "new type of {}({}) -> {}",
                    id_2.format_with_sym_table(st),
                    type_2.a_type.format_with_sym_table(st),
                    type_1.a_type.format_with_sym_table(st)
                );
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
                println!(
                    "new type of {}({}) -> {}",
                    id_1.format_with_sym_table(st),
                    type_1.a_type.format_with_sym_table(st),
                    type_2.a_type.format_with_sym_table(st)
                );
                st.set_type_of(id_1, &type_2.a_type)
            }
            st.union_atom(id_1, id_2);
            Ok(true)
        }
    }
}

pub fn unify_equal(c: &mut Chronicle, ch: &mut ChronicleHierarchy, _context: &ConversionContext) {
    let mut vec_constraint_to_rm = vec![];
    for (index, constraint) in c.get_constraints().iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                if let Ok(true) = bind_variables(id_1, id_2, &mut ch.sym_table) {
                    vec_constraint_to_rm.push(index);
                }
            }
        }
    }

    c.rm_set_constraint(vec_constraint_to_rm)
}

pub fn rm_useless_var(
    c: &mut Chronicle,
    ch: &mut ChronicleHierarchy,
    _context: &ConversionContext,
) {
    //Variables in expressions
    let vars = c.get_variables();
    let parameters: HashSet<AtomId> = vars
        .iter()
        .filter_map(|v| {
            let v = ch.sym_table.get_parent(v);
            if ch.sym_table.get_type_of(v).unwrap().kind
                == AtomKind::Variable(VariableKind::Parameter)
            {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    let used_vars = c
        .get_all_variables_in_sets()
        .iter()
        .map(|a| *ch.sym_table.get_parent(a))
        .collect();
    c.rm_set_var(c.get_variables().iter().cloned().collect());
    let mut parent_vars = hashset![];
    for var in &vars {
        parent_vars.insert(*ch.sym_table.get_parent(var));
    }

    let parent_vars = parent_vars.intersection(used_vars);
    let parent_vars = parent_vars.union(parameters);
    c.add_variables(parent_vars);
}

pub fn simplify_timepoints(
    c: &mut Chronicle,
    ch: &mut ChronicleHierarchy,
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
        if matches!(constraint, Constraint::Neg(_)) {
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
