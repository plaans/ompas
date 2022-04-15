use crate::conversion::post_processing::post_processing;
use crate::conversion::pre_processing::pre_processing;
use crate::conversion::processing::{convert_lvalue_to_expression_chronicle, MetaData};
use aries_planning::chronicles::ChronicleKind;
use ompas_rae_structs::exec_context::rae_env::{Parameters, Task};
use ompas_rae_structs::planning::chronicle::ChronicleTemplate;
use ompas_rae_structs::planning::symbol_table::{AtomId, SymTable};
use ompas_rae_structs::planning::type_table::PlanningAtomType;
use ompas_rae_structs::planning::{ConversionCollection, ConversionContext};
use sompas_structs::lerror;
use sompas_structs::lerror::LError::SpecialError;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
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
) -> lerror::Result<ConversionCollection> {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let mut ch: ConversionCollection = Default::default();

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
    let obj_id = *ch.sym_table.get_type_id(PlanningAtomType::Object).unwrap();

    for (obj_type, objects) in &conversion_context.state.instance.inner {
        let type_sym = if let LValueS::List(vec) = obj_type {
            vec[1].clone()
        } else {
            panic!("should be (instance <type>), we have {}", obj_type)
        };

        //println!("definition of type: {}", type_sym);
        let type_id = ch
            .sym_table
            .declare_new_type(type_sym.clone(), Some(obj_id));

        /*println!(
            "type of {}: {}",
            type_sym,
            ch.sym_table
                .get_type_of(&type_id)
                .unwrap()
                .format_with_sym_table(&ch.sym_table)
        );*/
        if let LValueS::List(objects) = objects {
            for obj in objects {
                ch.sym_table
                    .declare_symbol(obj, Some(PlanningAtomType::Other(type_id)));
            }
        } else {
            panic!("should be a list")
        }
    }
    //panic!("for no fucking reason");
    //Add actions, tasks and methods symbols to ch.sym_table:

    //Add tasks to domain
    for task in conversion_context.domain.get_tasks().values() {
        ch.tasks.push(declare_task(task, &mut ch.sym_table));
    }

    for action in conversion_context.domain.get_actions().values() {
        //evaluate the lambda sim.
        let chronicle = convert_abstract_task_to_chronicle(
            &action.get_sim().try_into()?,
            action.get_label(),
            None,
            action.get_parameters(),
            &conversion_context,
            &mut ch,
            ChronicleKind::Action,
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
            ChronicleKind::Method,
        )?;

        ch.chronicle_templates.push(chronicle);
    }

    Ok(ch)
}

const CONVERT_ABSTRACT_TASK_TO_CHRONICLE: &str = "convert-abtract-task-to-chronicle";

pub fn convert_abstract_task_to_chronicle(
    lambda: &LLambda,
    label: impl Display,
    task: Option<&Task>,
    parameters: &Parameters,
    conversion_context: &ConversionContext,
    ch: &mut ConversionCollection,
    chronicle_kind: ChronicleKind,
) -> lerror::Result<ChronicleTemplate> {
    let symbol_id = ch.sym_table.declare_symbol(&label.to_string(), None);

    let copy_label = label.to_string();
    let mut chronicle = ChronicleTemplate::new(ch, label, chronicle_kind);
    let mut name = vec![
        /* *chronicle.get_presence(),
         *chronicle.get_result(),
         *chronicle.get_start(),
         *chronicle.get_end(),*/
        symbol_id,
    ];
    if let LambdaArgs::List(l) = lambda.get_params() {
        if l.len() != parameters.get_number() {
            return Err(SpecialError(
                CONVERT_ABSTRACT_TASK_TO_CHRONICLE,
                format!(
                    "for {}: definition of parameters are different({} != {})",
                    copy_label,
                    lambda.get_params(),
                    parameters
                ),
            ));
        }

        for (pl, (pt, t)) in l.iter().zip(parameters.inner().iter()) {
            assert_eq!(pl, pt);
            let type_id = *ch
                .sym_table
                .get_type_id(
                    t.try_as_single()
                        .expect("Only single types are handled for the moment."),
                )
                //    .unwrap();
                .unwrap_or_else(|| panic!("{} is not defined", t));

            let id = ch.sym_table.declare_new_parameter(
                pt,
                true,
                Some(PlanningAtomType::Other(type_id)),
            );
            chronicle.add_var(&id);
            name.push(id);
        }
    }
    chronicle.set_name(name.clone());
    chronicle.set_task(match task {
        Some(task) => {
            let mut task_name: Vec<AtomId> =
                name[0..task.get_parameters().get_number() + 1].to_vec();
            task_name[0] = *ch.sym_table.id(task.get_label()).unwrap();
            task_name
        }
        None => name,
    });

    let pre_processed = pre_processing(lambda.get_body(), conversion_context, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;

    chronicle.absorb_expression_chronicle(ec);
    /*chronicle.add_constraint(Constraint::LT(
        chronicle.get_start().into(),
        chronicle.get_end().into(),
    ));*/

    post_processing(&mut chronicle, conversion_context, ch)?;

    Ok(chronicle)
}

pub fn convert_lvalue_to_chronicle(
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ConversionCollection,
) -> lerror::Result<ChronicleTemplate> {
    //Creation and instantiation of the chronicle
    let label = "unnamed_chronicle";
    let symbol_id = ch
        .sym_table
        .declare_symbol(label, Some(PlanningAtomType::Task));

    let mut chronicle = ChronicleTemplate::new(ch, label, ChronicleKind::Method);
    let mut name = vec![
        /* *chronicle.get_presence(),
         *chronicle.get_result(),
         *chronicle.get_start(),
         *chronicle.get_end(),*/
        symbol_id,
    ];

    let lvalue: &LValue = if let LValue::Lambda(lambda) = exp {
        let params = lambda.get_params();
        match params {
            LambdaArgs::Sym(s) => {
                let id = ch.sym_table.declare_new_parameter(&s, true, None);
                chronicle.add_var(&id);
                name.push(id);
            }
            LambdaArgs::List(list) => {
                for param in list {
                    let id = ch.sym_table.declare_new_parameter(&param, true, None);
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

    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;
    chronicle.absorb_expression_chronicle(ec);

    post_processing(&mut chronicle, conversion_context, ch)?;

    chronicle.set_name(name.clone());
    chronicle.set_task(name);

    Ok(chronicle)
}

pub fn declare_task(task: &Task, st: &mut SymTable) -> Vec<AtomId> {
    let task_label_id = *st
        .id(task.get_label())
        .expect("symbol of task should be defined");

    /*let prez = st.declare_new_parameter(PREZ, true, Some(PlanningAtomType::Bool));
    let start = st.declare_new_parameter(START, true, Some(PlanningAtomType::Bool));
    let end = st.declare_new_parameter(END, true, Some(PlanningAtomType::Bool));

    let result = st.declare_new_parameter(RESULT, true, Some(PlanningAtomType::Bool));*/

    //let mut task_lit: Vec<AtomId> = vec![prez, result, start, end, task_label_id];
    let mut task_lit: Vec<AtomId> = vec![task_label_id];

    for (param, _t) in task.get_parameters().inner() {
        task_lit.push(st.declare_new_parameter(&param, true, None))
    }

    task_lit
}

pub fn build_chronicle(
    mut chronicle: ChronicleTemplate,
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ConversionCollection,
) -> lerror::Result<ChronicleTemplate> {
    let lvalue: &LValue = if let LValue::Lambda(lambda) = exp {
        lambda.get_body()
    } else {
        exp
    };

    let pre_processed = pre_processing(lvalue, conversion_context, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        conversion_context,
        ch,
        MetaData::new(true, false),
    )?;

    chronicle.absorb_expression_chronicle(ec);

    post_processing(&mut chronicle, conversion_context, ch)?;

    Ok(chronicle)
}
