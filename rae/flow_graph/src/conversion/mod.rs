use crate::conversion::chronicle::convert_method;
use crate::conversion::chronicle::post_processing::post_processing;
use crate::conversion::flow::convert_lv;
use crate::conversion::flow::post_processing::flow_graph_post_processing;
use crate::conversion::flow::pre_processing::pre_processing;
use ompas_rae_structs::acting_domain::parameters::Parameters;
use ompas_rae_structs::acting_domain::task::Task;
use ompas_rae_structs::conversion::chronicle::template::{ChronicleKind, ChronicleTemplate};
use ompas_rae_structs::conversion::context::ConversionContext;
use ompas_rae_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_rae_structs::planning::domain::{
    CommandChronicle, MethodChronicle, PlanningDomain, TaskChronicle,
};
use ompas_rae_structs::sym_table::r#ref::RefSymTable;
use ompas_rae_structs::sym_table::VarId;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::Display;

pub mod chronicle;
pub mod flow;

pub async fn convert(
    lv: &LValue,
    env: &LEnv,
    st: RefSymTable,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let lv = pre_processing(lv, env).await?;

    //let sym_table = RefCell::new(SymTable::default());

    let mut graph = FlowGraph::new(st);

    let flow = convert_lv(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_method(None, &mut graph, &flow)?;
    post_processing(&mut ch)?;

    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = lv;

    Ok(ch)
}

#[allow(unused)]
const CONVERT_LVALUE_TO_CHRONICLE: &str = "convert_lvalue_to_chronicle";
#[allow(unused)]
const CONVERT_DOMAIN_TO_CHRONICLE_HIERARCHY: &str = "convert_domain_to_chronicle_hierarchy";

pub async fn convert_acting_domain(
    cc: &ConversionContext,
) -> lruntimeerror::Result<PlanningDomain> {
    //for each action: translate to chronicle
    //for each method: translate to chronicle

    let st = cc.st.clone();

    //add new types to list of types.
    //panic!("for no fucking reason");
    //Add actions, tasks and methods symbols to ch.sym_table:

    //Add tasks to domain

    let mut tasks = vec![];
    let mut commands = vec![];
    let mut methods = vec![];
    let sf = cc.domain.get_state_functions().values().cloned().collect();

    for task in cc.domain.get_tasks().values() {
        tasks.push(declare_task(task, st.clone()));
    }

    for command in cc.domain.get_commands().values() {
        //evaluate the lambda sim.
        let template = convert_abstract_task_to_chronicle(
            &command.get_model().try_into()?,
            command.get_label(),
            None,
            command.get_parameters(),
            &cc,
            ChronicleKind::Command,
        )
        .await?;

        commands.push(CommandChronicle {
            command: command.clone(),
            template,
        });
    }

    //Add all methods to the domain
    for method in cc.domain.get_methods().values() {
        let task = cc.domain.get_tasks().get(method.get_task_label()).unwrap();

        let method_lambda: LLambda = method.get_body().try_into().expect("");

        let template = convert_abstract_task_to_chronicle(
            &method_lambda,
            &method.label,
            Some(task),
            method.get_parameters(),
            &cc,
            ChronicleKind::Method,
        )
        .await?;

        methods.push(MethodChronicle {
            method: method.clone(),
            template,
        });
    }

    Ok(PlanningDomain {
        sf,
        tasks,
        methods,
        commands,
        new_types: vec![],
        st,
    })
}

const CONVERT_ABSTRACT_TASK_TO_CHRONICLE: &str = "convert-abtract-task-to-chronicle";

pub async fn convert_abstract_task_to_chronicle(
    lambda: &LLambda,
    label: impl Display,
    task: Option<&Task>,
    parameters: &Parameters,
    cc: &ConversionContext,
    kind: ChronicleKind,
) -> lruntimeerror::Result<ChronicleTemplate> {
    let st = cc.st.clone();

    let symbol_id = st.get_sym_id(&label.to_string()).unwrap();

    let mut ch = ChronicleTemplate::new(label.to_string(), kind, st.clone());
    let mut name: Vec<VarId> = vec![symbol_id];
    if let LambdaArgs::List(l) = lambda.get_params() {
        if l.len() != parameters.get_number() {
            return Err(lruntimeerror!(
                CONVERT_ABSTRACT_TASK_TO_CHRONICLE,
                format!(
                    "for {}: definition of parameters are different({} != {})",
                    label,
                    lambda.get_params(),
                    parameters
                )
            ));
        }

        for (pl, (pt, t)) in l.iter().zip(parameters.inner().iter()) {
            assert_eq!(pl, pt);

            let id = st.new_parameter(pt, t.get_domain().clone());
            ch.add_var(id);
            name.push(id);
        }
    }
    ch.set_name(name.clone());
    ch.set_task(match task {
        Some(task) => {
            let mut task_name: Vec<VarId> =
                name[0..task.get_parameters().get_number() + 1].to_vec();
            task_name[0] = st.get_sym_id(task.get_label()).unwrap();
            task_name
        }
        None => name,
    });

    let lv = lambda.get_body();

    let lv = pre_processing(lv, &cc.env).await?;

    let mut graph = FlowGraph::new(st);

    let flow = convert_lv(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_method(Some(ch), &mut graph, &flow)?;
    post_processing(&mut ch)?;

    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = lv;

    Ok(ch)
}

/*
pub fn convert_lvalue_to_chronicle(
    exp: &LValue,
    cc: &ConversionContext,
) -> lruntimeerror::Result<ChronicleTemplate> {
    //Creation and instantiation of the chronicle
    let label = "unnamed_chronicle";
    let symbol_id = st
        .new_consta(label, Some(PlanningAtomType::Task));

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

    let pre_processed = pre_processing(lvalue, cc, ch)?;

    chronicle.set_debug(Some(pre_processed.clone()));

    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        cc,
        ch,
        MetaData::new(true, false),
    )?;
    chronicle.absorb_expression_chronicle(ec);

    post_processing(&mut chronicle, cc, ch)?;

    chronicle.set_name(name.clone());
    chronicle.set_task(name);

    Ok(chronicle)
}*/

pub fn declare_task(task: &Task, st: RefSymTable) -> TaskChronicle {
    let task_label_id = st
        .get_sym_id(task.get_label())
        .expect("symbol of task should be defined");

    /*let prez = st.declare_new_parameter(PREZ, true, Some(PlanningAtomType::Bool));
    let start = st.declare_new_parameter(START, true, Some(PlanningAtomType::Bool));
    let end = st.declare_new_parameter(END, true, Some(PlanningAtomType::Bool));

    let result = st.declare_new_parameter(RESULT, true, Some(PlanningAtomType::Bool));*/

    //let mut task_lit: Vec<AtomId> = vec![prez, result, start, end, task_label_id];
    let mut task_lit: Vec<VarId> = vec![task_label_id];

    for (param, t) in task.get_parameters().inner() {
        task_lit.push(st.new_parameter(param, t.get_domain().clone()))
    }
    TaskChronicle {
        task: task.clone(),
        convert: task_lit,
    }
}

/*
pub fn build_chronicle(
    mut chronicle: ChronicleTemplate,
    exp: &LValue,
    conversion_context: &ConversionContext,
    ch: &mut ConversionCollection,
) -> lruntimeerror::Result<ChronicleTemplate> {
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
}*/
