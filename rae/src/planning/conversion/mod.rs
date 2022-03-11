use crate::context::rae_env::{Parameters, Task};
use crate::planning::conversion::post_processing::*;
use crate::planning::conversion::pre_processing::pre_processing;
use crate::planning::conversion::processing::{convert_lvalue_to_expression_chronicle, MetaData};
use crate::planning::structs::chronicle::Chronicle;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::SymTable;
use crate::planning::structs::type_table::PlanningAtomType;
use crate::planning::structs::{ChronicleHierarchy, ConversionContext};
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::llambda::{LLambda, LambdaArgs};
use ompas_lisp::core::structs::lvalue::LValue;
use std::convert::TryInto;
use std::fmt::Display;

pub mod post_processing;
pub mod pre_processing;
pub mod processing;

#[allow(unused)]
const CONVERT_LVALUE_TO_CHRONICLE: &str = "convert_lvalue_to_chronicle";
#[allow(unused)]
const CONVERT_DOMAIN_TO_CHRONICLE_HIERARCHY: &str = "convert_domain_to_chronicle_hierarchy";

pub fn convert_domain_to_chronicle_hierarchy(
    conversion_context: ConversionContext,
) -> Result<ChronicleHierarchy, LError> {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let mut ch: ChronicleHierarchy = Default::default();

    let actions: Vec<&str> = conversion_context
        .domain
        .get_actions()
        .keys()
        .map(|s| s.as_str())
        .collect();
    let tasks: Vec<&str> = conversion_context
        .domain
        .get_tasks()
        .keys()
        .map(|s| s.as_str())
        .collect();
    let state_functions = conversion_context
        .domain
        .get_state_functions()
        .keys()
        .map(|s| s.as_str())
        .collect();
    let methods = conversion_context
        .domain
        .get_methods()
        .keys()
        .map(|s| s.as_str())
        .collect();

    ch.sym_table
        .add_list_of_symbols_of_same_type(actions, Some(PlanningAtomType::Action))?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(state_functions, Some(PlanningAtomType::StateFunction))?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(methods, Some(PlanningAtomType::Method))?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(tasks, Some(PlanningAtomType::Task))?;

    //add new types to list of types.

    //Add actions, tasks and methods symbols to ch.sym_table:
    //let mut tasks_lits = HashMap::new();

    //Add tasks to domain
    for (_, task) in conversion_context.domain.get_tasks() {
        ch.tasks.push(declare_task(task, &mut ch.sym_table));
    }

    for (action_label, action) in conversion_context.domain.get_actions() {
        //evaluate the lambda sim.
        let mut chronicle = convert_abstract_task_to_chronicle(
            &action.get_sim().try_into()?,
            action_label,
            None,
            action.get_parameters(),
            &conversion_context,
            &mut ch,
        )?;

        ch.chronicle_templates.push(chronicle);
    }

    //Add all methods to the domain
    for (method_label, method) in conversion_context.domain.get_methods() {
        let task = conversion_context
            .domain
            .get_tasks()
            .get(method.get_task_label())
            .unwrap();

        let method_lambda: LLambda = method.get_lambda().try_into().expect("");

        let chronicle = convert_abstract_task_to_chronicle(
            &method_lambda,
            method_label,
            Some(task),
            method.get_parameters(),
            &conversion_context,
            &mut ch,
        )?;

        ch.chronicle_templates.push(chronicle);
    }

    ch.problem = (&conversion_context).into();

    Ok(ch)
}

pub fn convert_abstract_task_to_chronicle(
    lambda: &LLambda,
    label: impl Display,
    task: Option<&Task>,
    parameters: &Parameters,
    conversion_context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<Chronicle, LError> {
    let symbol_id = ch.sym_table.declare_new_symbol(&label.to_string(), None);

    let mut chronicle = Chronicle::new(ch, label);
    let mut name = vec![
        *chronicle.get_prez(),
        *chronicle.get_result(),
        *chronicle.get_start(),
        *chronicle.get_end(),
        symbol_id,
    ];
    if let LambdaArgs::List(l) = lambda.get_params() {
        assert_eq!(l.len(), parameters.get_number());

        for (pl, (pt, t)) in l.iter().zip(parameters.inner().iter()) {
            assert_eq!(pl, pt);
            let id = ch.sym_table.declare_new_variable(pt, true, None);
            chronicle.add_var(&id);
            name.push(id);
        }
    }

    let pre_processed = pre_processing(lambda.get_body(), conversion_context, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let mut ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;

    post_processing(&mut ec, conversion_context, ch)?;

    chronicle.absorb_expression_chronicle(ec, &mut ch.sym_table);
    chronicle.set_name(name.clone().into());
    chronicle.set_task(match task {
        Some(task) => declare_task(task, &mut ch.sym_table),
        None => name.into(),
    });

    Ok(chronicle)
}

pub fn convert_lvalue_to_chronicle(
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<Chronicle, LError> {
    //Creation and instantiation of the chronicle
    let label = "unnamed_chronicle";
    let symbol_id = ch.sym_table.declare_new_symbol(label, None);

    let mut chronicle = Chronicle::new(ch, label);
    let mut name = vec![
        *chronicle.get_prez(),
        *chronicle.get_result(),
        *chronicle.get_start(),
        *chronicle.get_end(),
        symbol_id,
    ];

    let lvalue: &LValue = if let LValue::Lambda(lambda) = exp {
        let params = lambda.get_params();
        match params {
            LambdaArgs::Sym(s) => {
                let id = ch.sym_table.declare_new_variable(&s, true, None);
                chronicle.add_var(&id);
                name.push(id);
            }
            LambdaArgs::List(list) => {
                for param in list {
                    let id = ch.sym_table.declare_new_variable(&param, true, None);
                    chronicle.add_var(&id);
                    name.push(id);
                }
            }
            LambdaArgs::Nil => {}
        }

        lambda.get_body()
    } else {
        exp
    };

    let pre_processed = pre_processing(lvalue, conversion_context, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let mut ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;

    post_processing(&mut ec, conversion_context, ch)?;

    chronicle.absorb_expression_chronicle(ec, &mut ch.sym_table);
    chronicle.set_name(name.clone().into());
    chronicle.set_task(name.into());

    Ok(chronicle)
}

pub fn declare_task(task: &Task, st: &mut SymTable) -> Lit {
    let task_label_id = *st
        .id(task.get_label())
        .expect("symbol of task should be defined");

    let task_label = task.get_label();

    let prez = st.declare_new_variable(
        format!("{}_prez", task_label),
        true,
        Some(PlanningAtomType::Bool),
    );
    let start = st.declare_new_variable(
        format!("{}_start", task_label),
        true,
        Some(PlanningAtomType::Bool),
    );
    let end = st.declare_new_variable(
        format!("{}_end", task_label),
        true,
        Some(PlanningAtomType::Bool),
    );

    let result = st.declare_new_variable(
        format!("{}_result", task_label),
        true,
        Some(PlanningAtomType::Bool),
    );

    let mut task_lit: Vec<Lit> = vec![
        prez.into(),
        result.into(),
        start.into(),
        end.into(),
        task_label_id.into(),
    ];

    for (param, t) in task.get_parameters().inner() {
        //TODO: add types for parameters
        task_lit.push(st.declare_new_variable(&param, true, None).into())
    }

    task_lit.into()
}

pub fn build_chronicle(
    mut chronicle: Chronicle,
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<Chronicle, LError> {
    let lvalue: &LValue = if let LValue::Lambda(lambda) = exp {
        lambda.get_body()
    } else {
        exp
    };

    let pre_processed = pre_processing(lvalue, conversion_context, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let mut ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;

    post_processing(&mut ec, conversion_context, ch)?;

    chronicle.absorb_expression_chronicle(ec, &mut ch.sym_table);
    Ok(chronicle)
}
