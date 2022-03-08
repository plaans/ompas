use crate::planning::conversion::post_processing::*;
use crate::planning::conversion::pre_processing::pre_processing;
use crate::planning::conversion::processing::{convert_lvalue_to_expression_chronicle, MetaData};
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::chronicle::Chronicle;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::{ChronicleHierarchy, ConversionContext};
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::llambda::LambdaArgs;
use ompas_lisp::core::structs::lvalue::LValue;

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

    let actions: Vec<String> = conversion_context
        .domain
        .get_actions()
        .keys()
        .cloned()
        .collect();
    let tasks: Vec<String> = conversion_context
        .domain
        .get_tasks()
        .keys()
        .cloned()
        .collect();
    let state_functions = conversion_context
        .domain
        .get_state_functions()
        .keys()
        .cloned()
        .collect();
    let methods = conversion_context
        .domain
        .get_methods()
        .keys()
        .cloned()
        .collect();

    ch.sym_table
        .add_list_of_symbols_of_same_type(actions, &AtomType::Action)?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(state_functions, &AtomType::StateFunction)?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(methods, &AtomType::Method)?;
    ch.sym_table
        .add_list_of_symbols_of_same_type(tasks, &AtomType::Task)?;

    //Add actions, tasks and methods symbols to ch.sym_table:
    //let mut tasks_lits = HashMap::new();

    //Add tasks to domain
    for (task_label, task) in conversion_context.domain.get_tasks() {
        let mut task_lit: Vec<Lit> = vec![ch
            .sym_table
            .id(task_label)
            .expect("symbol of task should be defined")
            .into()];

        for param in task.get_parameters().get_params() {
            task_lit.push(ch.sym_table.declare_new_symbol(&param, true, true).into())
        }
        task_lit.push(ch.sym_table.declare_new_result().get_id().into());

        let task_lit: Lit = task_lit.into();
        ch.tasks.push(task_lit);
    }

    for (action_label, action) in conversion_context.domain.get_actions() {
        //evaluate the lambda sim.
        let mut chronicle =
            convert_lvalue_to_chronicle(action.get_sim(), &conversion_context, &mut ch)?;
        let symbol_id = *ch
            .sym_table
            .id(action_label)
            .unwrap_or_else(|| panic!("{} was not well defined", action_label));
        let mut name = vec![symbol_id];

        for e in action.get_parameters().get_params() {
            let symbol_id = *ch.sym_table.id(&e).unwrap_or_else(|| {
                panic!(
                    "parameters of {} were not defined in the chronicle",
                    action_label
                )
            });
            name.push(symbol_id);
            chronicle.add_var(&symbol_id);
        }

        name.push(*chronicle.get_result_id());

        chronicle.set_name(name.into());
        ch.actions.push(chronicle);
    }

    //Add all methods to the domain
    for (method_label, method) in conversion_context.domain.get_methods() {
        let chronicle = Chronicle::new_method(method_label, method, &conversion_context, &mut ch)?;

        ch.methods.push(chronicle);
    }

    ch.problem = (&conversion_context).into();

    Ok(ch)
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

pub fn convert_lvalue_to_chronicle(
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ChronicleHierarchy,
) -> Result<Chronicle, LError> {
    //Creation and instantiation of the chronicle

    let mut chronicle = Chronicle::new(ch);

    let lvalue: &LValue = if let LValue::Lambda(lambda) = exp {
        let params = lambda.get_params();
        match params {
            LambdaArgs::Sym(s) => {
                ch.sym_table.declare_new_symbol(&s, true, true);
            }
            LambdaArgs::List(list) => {
                for param in list {
                    ch.sym_table.declare_new_symbol(&param, true, true);
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

    Ok(chronicle)
}
