use crate::conversion::post_processing::post_processing;
use crate::conversion::pre_processing::pre_processing;
use crate::conversion::processing::translate_lvalue_to_expression_chronicle;
use crate::structs::atom::AtomType;
use crate::structs::chronicle::Chronicle;
use crate::structs::lit::Lit;
use crate::structs::symbol_table::SymTable;
use crate::structs::{ConversionContext, Domain};
use im::HashMap;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use ompas_lisp::core::structs::llambda::LambdaArgs;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;

pub mod post_processing;
pub mod pre_processing;
pub mod processing;

const TRANSLATE_LVALUE_TO_CHRONICLE: &str = "translate_lvalue_to_chronicle";
const TRANSFORM_DOMAIN_ENV_TO_HIERARCHY: &str = "transform_domain_env_to_hierarchy";

pub fn translate_domain_env_to_hierarchy(
    context: ConversionContext,
) -> Result<(Domain, SymTable), LError> {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let mut symbol_table = SymTable::default();

    let actions: Vec<String> = context.domain.get_actions().keys().cloned().collect();
    let tasks: Vec<String> = context.domain.get_tasks().keys().cloned().collect();
    let state_functions = context
        .domain
        .get_state_functions()
        .keys()
        .cloned()
        .collect();
    let methods = context.domain.get_methods().keys().cloned().collect();

    symbol_table.add_list_of_symbols_of_same_type(actions, &AtomType::Action)?;
    symbol_table.add_list_of_symbols_of_same_type(state_functions, &AtomType::StateFunction)?;
    symbol_table.add_list_of_symbols_of_same_type(methods, &AtomType::Method)?;
    symbol_table.add_list_of_symbols_of_same_type(tasks, &AtomType::Task)?;

    //Add actions, tasks and methods symbols to symbol_table:
    let mut methods = vec![];
    let mut tasks_lits = HashMap::new();
    let mut actions = vec![];

    //Add tasks to domain
    for (task_label, task) in context.domain.get_tasks() {
        let mut task_lit: Vec<Lit> = vec![symbol_table
            .id(task_label)
            .expect("symbol of task should be defined")
            .into()];

        for param in task.get_parameters().get_params() {
            task_lit.push(symbol_table.declare_new_symbol(param, true).into())
        }
        tasks_lits.insert(task_label, Lit::from(task_lit));
    }

    for (action_label, action) in context.domain.get_actions() {
        //evaluate the lambda sim.
        let mut chronicle =
            translate_lvalue_to_chronicle(action.get_sim(), &context, &mut symbol_table)?;
        let symbol_id = *symbol_table
            .id(action_label)
            .unwrap_or_else(|| panic!("{} was not well defined", action_label));
        let mut name = vec![symbol_id];

        for e in action.get_parameters().get_params() {
            let symbol_id = *symbol_table
                .id(&e)
                .expect("parameters were not defined in the chronicle");
            name.push(symbol_id);
            chronicle.add_var(&symbol_id);
        }

        chronicle.set_name(name.into());

        actions.push(chronicle)
    }

    //Add all methods to the domain
    for (method_label, method) in context.domain.get_methods() {
        let mut chronicle =
            translate_lvalue_to_chronicle(method.get_body(), &context, &mut symbol_table)?;

        /*let pre_conditions = translate_lvalue_to_expression_chronicle(
            method.get_pre_conditions(),
            &context,
            &mut symbol_table,
        )?;*/

        //chronicle.absorb_expression_chronicle(pre_conditions);

        let task = tasks_lits.get(method.get_task_label()).unwrap();
        let symbol_id = *symbol_table
            .id(method_label)
            .unwrap_or_else(|| panic!("{} was not well defined", method_label));
        let mut name = vec![symbol_id.into()];

        if let Lit::Exp(exp) = task {
            chronicle.set_task(task.clone());
            for param in &exp[1..] {
                if let Lit::Atom(atom) = param {
                    chronicle.add_var(atom);
                    name.push(param.clone());
                } else {
                    return Err(SpecialError(
                        TRANSFORM_DOMAIN_ENV_TO_HIERARCHY,
                        "parameters of a task should be atoms".to_string(),
                    ));
                }
            }
            for e in &method.get_parameters().get_params()[exp.len() - 1..] {
                let symbol_id = *symbol_table
                    .id(e)
                    .expect("parameters were not defined in the chronicle");
                name.push(symbol_id.into());
                chronicle.add_var(&symbol_id);
            }
        } else {
            return Err(SpecialError(
                TRANSFORM_DOMAIN_ENV_TO_HIERARCHY,
                "".to_string(),
            ));
        }

        chronicle.set_name(name.into());
        methods.push(chronicle);
    }

    Ok((
        Domain::new(actions, tasks_lits.values().cloned().collect(), methods),
        symbol_table,
    ))
}

pub fn translate_lvalue_to_chronicle(
    exp: &LValue,
    context: &ConversionContext,
    symbol_table: &mut SymTable,
) -> Result<Chronicle, LError> {
    //Creation and instantiation of the chronicle

    if let LValue::Lambda(lambda) = exp {
        let mut chronicle = Chronicle::default();
        let params = lambda.get_params();
        match params {
            LambdaArgs::Sym(s) => {
                symbol_table.declare_new_symbol(s, true);
            }
            LambdaArgs::List(list) => {
                for param in list {
                    symbol_table.declare_new_symbol(param, true);
                }
            }
            LambdaArgs::Nil => {}
        }
        let body = pre_processing(&lambda.get_body(), context)?;

        chronicle.set_debug(Some(body.clone()));

        let mut ec = translate_lvalue_to_expression_chronicle(&body, context, symbol_table)?;

        post_processing(&mut ec, symbol_table, context);

        chronicle.absorb_expression_chronicle(ec);
        Ok(chronicle)
    } else {
        Err(SpecialError(
            TRANSLATE_LVALUE_TO_CHRONICLE,
            format!(
                "chronicle can only be extracted from a lambda, here we have a {}.",
                TypeLValue::from(exp)
            ),
        ))
    }
}
