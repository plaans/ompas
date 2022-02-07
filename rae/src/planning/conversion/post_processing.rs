use crate::planning::point_algebra::path_consistency;
use crate::planning::point_algebra::problem::{Graph, Problem};
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::chronicle::{ChronicleSet, ExpressionChronicle};
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::{get_variables_of_type, ConversionContext};

use crate::planning::structs::traits::GetVariables;
use im::HashSet;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::SpecialError;

pub fn post_processing(
    ec: &mut ExpressionChronicle,
    sym_table: &mut SymTable,
    context: &ConversionContext,
) -> Result<(), LError> {
    unify_equal(ec, sym_table, context);
    rm_useless_var(ec, sym_table, context);
    simplify_timepoints(ec, sym_table, context)?;
    Ok(())
}

pub fn unify_equal(
    ec: &mut ExpressionChronicle,
    sym_table: &mut SymTable,
    _context: &ConversionContext,
) {
    let mut vec_constraint_to_rm = vec![];

    for (index, constraint) in ec.get_constraints().iter().enumerate() {
        if let Constraint::Eq(a, b) = constraint {
            if let (Lit::Atom(id_1), Lit::Atom(id_2)) = (a, b) {
                let type_1 = sym_table.get_type(id_1).expect("id should be defined");
                let type_2 = sym_table.get_type(id_2).expect("id should be defined");
                match (type_1, type_2) {
                    (
                        AtomType::Boolean | AtomType::Number,
                        AtomType::Boolean | AtomType::Number,
                    ) => {
                        assert_eq!(
                            sym_table.get_atom(id_1).unwrap(),
                            sym_table.get_atom(id_2).unwrap()
                        );
                    }
                    (AtomType::Number, AtomType::Timepoint) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Timepoint, AtomType::Number) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Boolean | AtomType::Number, _) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (_, AtomType::Boolean | AtomType::Number) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Symbol, AtomType::Object | AtomType::Result) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Object | AtomType::Result, AtomType::Symbol) => {
                        sym_table.union_atom(id_2, id_1);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Result, AtomType::Result) => {
                        sym_table.union_atom(id_1, id_2);
                        vec_constraint_to_rm.push(index);
                    }
                    (AtomType::Timepoint, AtomType::Timepoint) => {
                        if id_1 < id_2 {
                            sym_table.union_atom(id_1, id_2);
                        } else {
                            sym_table.union_atom(id_2, id_1);
                        }
                        vec_constraint_to_rm.push(index);
                    }
                    (_, _) => {}
                }
            }
        }
    }

    ec.rm_set_constraint(vec_constraint_to_rm)
}

pub fn rm_useless_var(
    ec: &mut ExpressionChronicle,
    sym_table: &mut SymTable,
    _context: &ConversionContext,
) {
    let mut vec = vec![];

    for var in ec.get_variables() {
        if var != sym_table.get_parent(&var) {
            vec.push(var)
        }
    }

    ec.rm_set_var(vec)
}

pub fn simplify_timepoints(
    ec: &mut ExpressionChronicle,
    sym_table: &mut SymTable,
    _: &ConversionContext,
) -> Result<(), LError> {
    let timepoints: HashSet<AtomId> = get_variables_of_type(
        ec.get_variables()
            .iter()
            .map(|a| sym_table.get_parent(a))
            .collect(),
        sym_table,
        AtomType::Timepoint,
    );
    let used_timepoints: HashSet<AtomId> = get_variables_of_type(
        ec.get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
        .iter()
        .map(|a| sym_table.get_parent(a))
        .collect(),
        sym_table,
        AtomType::Timepoint,
    );

    let _optional_timepoints: HashSet<AtomId> = timepoints.clone().difference(used_timepoints);
    let mut relations = vec![];
    for constraint in ec.get_constraints() {
        if matches!(constraint, Constraint::Neg(_)) {
        } else if let Ok(relation) = constraint.try_into_pa_relation(sym_table) {
            relations.push(relation)
        }
    }

    let problem: Problem<AtomId> = Problem::new(timepoints.iter().cloned().collect(), relations);

    let graph: Graph<AtomId> = (&problem).into();

    graph.print();

    match path_consistency(graph) {
        Ok(m) => m.print(),
        Err(_) => {
            let err: LError = SpecialError(
                "",
                "Error in graph. Set of constraints is not consistent.".to_string(),
            );

            println!("{:?}", err);
            //return Err(err)
        }
    };

    Ok(())
    /*println!("not used timepoints : {}", {
        let mut string = "{".to_string();
        for (i, t) in optional_timepoints.iter().enumerate() {
            if i != 0 {
                string.push(',');
            }
            string.push_str(sym_table.get_atom(t).unwrap().to_string().as_str())
        }
        string.push('}');
        string
    })*/
}
