use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::lit::{lvalue_to_lit, Lit};
use crate::model::chronicle::{Chronicle, ChronicleSet};
use crate::model::sym_domain::basic_type::BasicType;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, GetVariables};
use crate::model::sym_table::{EmptyDomains, VarId};
use crate::planning::conversion::point_algebra::problem::{
    try_into_pa_relation, PAGraph, PAProblem,
};
use crate::planning::conversion::point_algebra::remove_useless_timepoints;
use im::HashSet;
use ompas_language::sym_table::TYPE_TIMEPOINT;
use sompas_core::{eval, parse};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::fmt::Write;
use std::ops::Deref;
const TRY_EVAL_APPLY: &str = "try_eval_apply";

pub async fn post_processing(c: &mut Chronicle, env: &LEnv) -> Result<(), LRuntimeError> {
    c.st.flat_bindings();
    merge_conditions(c)?;
    try_eval_apply(c, env).await?;
    simplify_constraints(c)?;
    rm_useless_var(c);
    simplify_timepoints(c)?;
    rm_useless_var(c);
    c.flat_bindings();
    Ok(())
}

pub fn rm_useless_var(c: &mut Chronicle) {
    //Variables in expressions
    c.flat_bindings();
    let parameters: HashSet<VarId> = c
        .get_variables()
        .iter()
        .filter_map(|v| {
            if c.st.get_variable(v).is_parameter() {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    let used_vars: HashSet<VarId> = c
        .get_all_variables_in_sets()
        .iter()
        .filter_map(|v| {
            if !c.st.get_domain_of_var(v).is_constant() {
                Some(*v)
            } else {
                None
            }
        })
        .collect();
    c.variables.clear();

    let new_vars = used_vars.union(parameters);
    for v in &new_vars {
        assert_eq!(*v, c.st.get_var_parent(v));
    }
    for v in new_vars {
        c.add_var(v)
    }
}

pub fn simplify_timepoints(c: &mut Chronicle) -> Result<(), LRuntimeError> {
    let st = c.st.clone();
    let _format_hash = |set: &HashSet<VarId>| -> String {
        let mut str = "{".to_string();
        for (i, e) in set.iter().enumerate() {
            if i != 0 {
                str.push(',');
            }
            write!(str, "{}", e.format(&st, true)).unwrap();
        }
        write!(str, "}}").unwrap();
        str
    };
    let timepoint_domain = st.get_type_as_domain(TYPE_TIMEPOINT).unwrap();

    let mut relations = vec![];
    let mut index_temporal_constraints = vec![];
    for (i, constraint) in c.get_constraints().iter().enumerate() {
        if matches!(constraint, Constraint::Not(_)) {
        } else if let Ok(r) = try_into_pa_relation(constraint, &st) {
            index_temporal_constraints.push(i);
            relations.push(r);
        }
    }

    c.rm_set_constraint(index_temporal_constraints);

    let timepoints: HashSet<VarId> = c
        .get_variables()
        .iter()
        .map(|a| st.get_var_parent(a))
        .filter(|a| st.contained_in_domain(&st.get_domain_of_var(a), &timepoint_domain))
        .collect();

    //println!("timepoints: {:?}", format_hash(&timepoints));

    let used_timepoints: HashSet<VarId> = c
        .get_variables_in_sets(vec![
            ChronicleSet::Constraint,
            ChronicleSet::Effect,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
        .iter()
        .map(|a| st.get_var_parent(a))
        .filter(|a| st.contained_in_domain(&st.get_domain_of_var(a), &timepoint_domain))
        .collect();

    //println!("used timepoints: {}", format_hash(&used_timepoints));

    let hard_timepoints: HashSet<VarId> = c
        .get_variables()
        .iter()
        .map(|a| st.get_var_parent(a))
        .filter(|a| {
            let is_parameter = st.get_variable(a).is_parameter();
            let is_timepoint = st.contained_in_domain(&st.get_domain_of_var(a), &timepoint_domain);
            is_parameter && is_timepoint
        })
        .collect();

    //println!("hard<timepoints>: {}", format_hash(&hard_timepoints));

    let used_timepoints = used_timepoints.union(hard_timepoints);

    let optional_timepoints: HashSet<VarId> = timepoints.clone().difference(used_timepoints);

    //println!("opt<timepoints>: {}", format_hash(&optional_timepoints));
    let mut timepoints: Vec<VarId> = timepoints.iter().cloned().collect();
    timepoints.sort();
    let timepoints = timepoints
        .iter()
        .map(|a| (*a, optional_timepoints.contains(a)))
        .collect();
    //println!("st: {}", ch.get_mut_sym_table());

    let problem: PAProblem<VarId> = PAProblem::new(timepoints, relations);

    //println!("problem: {:?}", problem);

    let graph: PAGraph<VarId> = problem.borrow().into();
    //graph.print();
    let new_graph = remove_useless_timepoints(graph)?;
    //new_graph.print();

    let problem: PAProblem<VarId> = new_graph.into();
    for r in problem.get_relations() {
        c.add_constraint(r.into())
    }

    c.st.flat_bindings();

    Ok(())
}

pub fn simplify_constraints(c: &mut Chronicle) -> Result<(), LRuntimeError> {
    //simple case where
    let st = c.st.clone();

    loop {
        let mut vec: Vec<(usize, Constraint)> = vec![];
        let mut to_remove: Vec<usize> = vec![];
        for (i, c) in c.constraints.iter().enumerate() {
            if let Constraint::Eq(a, b) = c {
                match (a, b) {
                    //Simplify equality constraints between atoms
                    (Lit::Atom(a), Lit::Atom(b)) => {
                        let r = st.union_var(a, b);
                        if r.is_none() {
                            to_remove.push(i);
                        } else {
                            LRuntimeError::new("", "");
                        }
                    }
                    (Lit::Atom(a), Lit::Constraint(b)) | (Lit::Constraint(b), Lit::Atom(a)) => {
                        if st.contained_in_domain(&st.get_domain_of_var(a), &BasicType::True.into())
                        {
                            vec.push((i, b.deref().clone()));
                        }
                    }
                    _ => {}
                }
            }
            /*if let Constraint::Or(or) = c {
                if or.len() == 1 {
                    vec.push()
                }
            }*/
        }

        if vec.is_empty() && to_remove.is_empty() {
            break;
        }
        vec.drain(..).for_each(|(i, cons)| c.constraints[i] = cons);

        c.rm_set_constraint(to_remove);
    }

    Ok(())
}

pub fn merge_conditions(c: &mut Chronicle) -> Result<(), LRuntimeError> {
    let st = c.st.clone();

    let mut c_to_remove: HashSet<usize> = Default::default();

    for (i, c1) in c.get_conditions().iter().enumerate() {
        let next = i + 1;
        for (j, c2) in c.get_conditions()[next..].iter().enumerate() {
            let index = j + next;
            if c1.interval == c2.interval && c1.sv == c2.sv {
                st.union_var(&c1.value, &c2.value);
                c_to_remove.insert(index);
            }
        }
    }

    let mut vec: Vec<usize> = c_to_remove.iter().copied().collect();

    vec.sort_unstable();
    vec.reverse();
    vec.iter().for_each(|i| c.rm_condition(*i));

    c.st.flat_bindings();
    c.flat_bindings();
    Ok(())
}

pub async fn try_eval_apply(c: &mut Chronicle, env: &LEnv) -> Result<(), LRuntimeError> {
    let env = env.clone();
    let st = c.st.clone();

    let mut c_to_remove: Vec<usize> = Default::default();

    'loop_constraint: for (i, constraint) in c.constraints.iter_mut().enumerate() {
        if let Constraint::Eq(Lit::Atom(r_c), b) = constraint {
            if let Lit::Apply(args) = b {
                let mut args = args.clone();
                args.flat_bindings(&st);
                for arg in &args {
                    if !st.get_domain_of_var(arg).is_constant() {
                        continue 'loop_constraint;
                    }
                }
                let expr = args.format(&st, true);
                //print!("apply{expr}");
                let mut env = env.clone();
                let lv: LValue = {
                    let lv = parse(expr.as_str(), &mut env).await?;
                    eval(&lv, &mut env, None).await
                }?;
                //println!("=> {lv}");

                let lit = lvalue_to_lit(&lv, &st)?;
                match lit {
                    Lit::Atom(a) => {
                        if let EmptyDomains::Some(_) = st.union_var(r_c, &a) {
                            return Err(LRuntimeError::new(
                                TRY_EVAL_APPLY,
                                format!(
                                    "Simplification of {} did not worked, empty domain of result.",
                                    b.format(&st, true)
                                ),
                            ));
                        } else {
                            c_to_remove.push(i)
                        }
                    }
                    _ => {
                        *b = lit;
                    }
                }
            }
        }
    }

    let mut vec: Vec<usize> = c_to_remove.to_vec();

    vec.reverse();
    vec.iter().for_each(|i| c.rm_constraint(*i));

    c.st.flat_bindings();
    c.flat_bindings();
    Ok(())
}
