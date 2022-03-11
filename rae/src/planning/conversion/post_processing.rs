use crate::planning::point_algebra::problem::{Graph, Problem};
use crate::planning::point_algebra::remove_useless_timepoints;
use crate::planning::structs::chronicle::{ChronicleSet, ExpressionChronicle};
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::GetVariables;
use crate::planning::structs::type_table::{AtomKind, PlanningAtomType, VariableKind};
use crate::planning::structs::{ChronicleHierarchy, ConversionContext};
use im::{hashset, HashSet};
use ompas_lisp::core::structs::lerror::LError;

pub fn post_processing(
    ec: &mut ExpressionChronicle,
    context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<(), LError> {
    unify_equal(ec, ch, context);
    ch.sym_table.flat_bindings();
    simplify_timepoints(ec, ch, context)?;
    rm_useless_var(ec, ch, context);
    Ok(())
}

/// Returns true if the constraint can be safely deleted
pub fn bind_variables(
    id_1: &AtomId,
    id_2: &AtomId,
    sym_table: &mut SymTable,
) -> Result<bool, LError> {
    let type_1 = *sym_table.get_type_of(id_1).expect("id should be defined");
    let type_2 = *sym_table.get_type_of(id_2).expect("id should be defined");

    match (type_1.kind, type_2.kind) {
        (AtomKind::Constant, AtomKind::Constant) => {
            if type_1.a_type != type_2.a_type {
                return Err(Default::default());
            }
            if sym_table.get_atom(id_1).unwrap() != sym_table.get_atom(id_2).unwrap() {
                return Err(Default::default());
            }
            sym_table.union_atom(id_1, id_2);
            Ok(true)
        }
        (AtomKind::Constant, AtomKind::Variable(_)) => {
            if type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            }
            sym_table.union_atom(id_1, id_2);
            Ok(true)
        }
        (AtomKind::Variable(_), AtomKind::Constant) => {
            if type_1.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            }
            sym_table.union_atom(id_2, id_1);
            Ok(true)
        }
        (
            AtomKind::Variable(VariableKind::Parameter),
            AtomKind::Variable(VariableKind::Parameter),
        ) => {
            match (type_1.a_type.is_some(), type_2.a_type.is_some()) {
                (true, true) => {
                    if type_1.a_type.unwrap() == type_2.a_type.unwrap() {
                        sym_table.union_atom(id_1, id_2);
                    } else {
                        return Err(Default::default());
                    }
                }
                (true, false) => sym_table.union_atom(id_1, id_2),
                (false, true) => sym_table.union_atom(id_2, id_1),
                (false, false) => sym_table.union_atom(id_1, id_2),
            }
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Result), AtomKind::Variable(VariableKind::Result)) => {
            match (type_1.a_type.is_some(), type_2.a_type.is_some()) {
                (true, true) => {
                    if type_1.a_type.unwrap() == type_2.a_type.unwrap() {
                        sym_table.union_atom(id_1, id_2);
                    } else {
                        return Err(Default::default());
                    }
                }
                (true, false) => sym_table.union_atom(id_1, id_2),
                (false, true) => sym_table.union_atom(id_2, id_1),
                (false, false) => sym_table.union_atom(id_1, id_2),
            }
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Result), AtomKind::Variable(VariableKind::Parameter)) => {
            if type_1.a_type.is_some() && type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            } else if type_1.a_type.is_some() {
                sym_table.set_type_of(id_2, &type_1.a_type)
            }
            sym_table.union_atom(id_2, id_1);
            Ok(true)
        }
        (AtomKind::Variable(VariableKind::Parameter), AtomKind::Variable(VariableKind::Result)) => {
            if type_1.a_type.is_some() && type_2.a_type.is_some() {
                if type_1.a_type.unwrap() != type_2.a_type.unwrap() {
                    return Err(Default::default());
                }
            } else if type_2.a_type.is_some() {
                sym_table.set_type_of(id_1, &type_2.a_type)
            }
            sym_table.union_atom(id_1, id_2);
            Ok(true)
        }
        (_, _) => Ok(false),
    }
    /*{
        (AtomType::Boolean | AtomType::Number, AtomType::Boolean | AtomType::Number) => {
            assert_eq!(
                sym_table.get_atom(id_1).unwrap(),
                sym_table.get_atom(id_2).unwrap()
            );
            false
        }
        (AtomType::Number, AtomType::Timepoint) => {
            sym_table.union_atom(id_1, id_2);
            true
        }
        (AtomType::Timepoint, AtomType::Number) => {
            sym_table.union_atom(id_2, id_1);
            true
        }
        (AtomType::Boolean | AtomType::Number, _) => {
            sym_table.union_atom(id_1, id_2);
            true
        }
        (_, AtomType::Boolean | AtomType::Number) => {
            sym_table.union_atom(id_2, id_1);
            true
        }
        (AtomType::Variable(e1), AtomType::Result(e2)) => {
            if compatible_variable(e1, e2) {
                sym_table.union_atom(id_1, id_2);
                true
            } else {
                Err(Default::default())
            }
        }
        (AtomType::Result(v1), AtomType::Variable(v2)) => {
            sym_table.union_atom(id_2, id_1);
            true
        }
        (AtomType::Symbol, AtomType::Result(v1) | AtomType::Variable) => {
            sym_table.union_atom(id_1, id_2);
            true
        }
        (AtomType::Variable, AtomType::Variable) => {
            if id_1 < id_2 {
                sym_table.union_atom(id_1, id_2);
            } else {
                sym_table.union_atom(id_2, id_1);
            }
            true
        }
        (AtomType::Result | AtomType::Variable, AtomType::Symbol) => {
            sym_table.union_atom(id_2, id_1);
            true
        }
        (AtomType::Result, AtomType::Result) => {
            if id_1 < id_2 {
                sym_table.union_atom(id_1, id_2);
            } else {
                sym_table.union_atom(id_2, id_1);
            }
            true
        }
        (AtomType::Timepoint, AtomType::Timepoint) => {
            if id_1 < id_2 {
                sym_table.union_atom(id_1, id_2);
            } else {
                sym_table.union_atom(id_2, id_1);
            }
            true
        }
        (_, _) => false,
    }*/
}

pub fn unify_equal(
    ec: &mut ExpressionChronicle,
    ch: &mut ChronicleHierarchy,
    _context: &ConversionContext,
) {
    /*println!(
        "in unify equal for:\n{}",
        ec.format_with_sym_table(&ch.sym_table)
    );*/
    let mut vec_constraint_to_rm = vec![];
    for (index, constraint) in ec.get_constraints().iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                if let Ok(true) = bind_variables(id_1, id_2, &mut ch.sym_table) {
                    vec_constraint_to_rm.push(index);
                }
            }
        }
    }

    ec.rm_set_constraint(vec_constraint_to_rm)
}

pub fn rm_useless_var(
    ec: &mut ExpressionChronicle,
    ch: &mut ChronicleHierarchy,
    _context: &ConversionContext,
) {
    let vars = ec.get_variables();
    let used_vars = ec
        .get_all_variables_in_sets()
        .iter()
        .map(|a| ch.sym_table.get_parent(a))
        .collect();
    ec.rm_set_var(ec.get_variables().iter().cloned().collect());
    let mut parent_vars = hashset![];
    for var in &vars {
        parent_vars.insert(ch.sym_table.get_parent(var));
    }

    let parent_vars = parent_vars.intersection(used_vars);
    ec.add_variables(parent_vars);
}

pub fn simplify_timepoints(
    ec: &mut ExpressionChronicle,
    ch: &mut ChronicleHierarchy,
    _: &ConversionContext,
) -> Result<(), LError> {
    let timepoints: HashSet<AtomId> = ec
        .get_variables()
        .iter()
        .map(|a| ch.sym_table.get_parent(a))
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Timepoint)
        })
        .collect();
    let used_timepoints: HashSet<AtomId> = ec
        .get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
        .iter()
        .map(|a| ch.sym_table.get_parent(a))
        .filter(|a| {
            ch.sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Timepoint)
        })
        .collect();

    let optional_timepoints: HashSet<AtomId> = timepoints.clone().difference(used_timepoints);
    let mut relations = vec![];
    let mut index_temporal_constraints = vec![];
    for (i, constraint) in ec.get_constraints().iter().enumerate() {
        if matches!(constraint, Constraint::Neg(_)) {
        } else if let Ok(r) = constraint.try_into_pa_relation(&ch.sym_table) {
            index_temporal_constraints.push(i);
            relations.push(r);
        }
    }

    ec.rm_set_constraint(index_temporal_constraints);

    let mut timepoints: Vec<AtomId> = timepoints.iter().cloned().collect();
    timepoints.sort();
    let timepoints = timepoints
        .iter()
        .map(|a| (*a, optional_timepoints.contains(a)))
        .collect();
    let problem: Problem<AtomId> = Problem::new(timepoints, relations);
    /*println!(
        "temporal problem: {}",
        problem.format_with_sym_table(&ch.sym_table)
    );*/
    let graph: Graph<AtomId> = (&problem).into();
    /*println!(
        "temporal graph of : {}",
        ec.format_with_sym_table(&ch.sym_table)
    );*/
    //graph.print();

    /*match path_consistency(graph) {
        Ok(m) => {
            if get_debug() {
                println!(
                    "temporal graph of : {}",
                    ec.format_with_sym_table(&ch.sym_table)
                );
                m.print()
            }
        }
        Err(_) => {
            let err: LError = SpecialError(
                "",
                "Error in graph. Set of constraints is not consistent.".to_string(),
            );
            //println!("{:?}", err);
            return Err(err);
        }
    };*/

    /*println!("not used timepoints : {}", {
        let mut string = "{".to_string();
        for (i, t) in optional_timepoints.iter().enumerate() {
            if i != 0 {
                string.push(',');
            }
            string.push_str(ch.sym_table.get_atom(t).unwrap().to_string().as_str())
        }
        string.push('}');
        string
    });*/

    let new_graph = remove_useless_timepoints(graph)?;
    //println!("graph after removing:");
    //new_graph.print();

    let problem: Problem<AtomId> = new_graph.into();
    /*println!(
        "new temporal problem: {}",
        problem.format_with_sym_table(&ch.sym_table)
    );*/

    for r in problem.get_relations() {
        ec.add_constraint(r.into())
    }

    //panic!("no fucking reason");
    Ok(())
}
