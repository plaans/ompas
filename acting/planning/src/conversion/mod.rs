use crate::conversion::chronicle::convert_method;
use crate::conversion::chronicle::post_processing::post_processing;
use crate::conversion::flow::convert_lv;
use crate::conversion::flow::post_processing::flow_graph_post_processing;
use crate::conversion::flow::pre_processing::pre_processing;
use chrono::{DateTime, Utc};
use ompas_structs::acting_domain::parameters::Parameters;
use ompas_structs::acting_domain::task::Task;
use ompas_structs::conversion::chronicle::template::{ChronicleKind, ChronicleTemplate};
use ompas_structs::conversion::context::ConversionContext;
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::planning::domain::{
    CommandChronicle, MethodChronicle, PlanningDomain, TaskChronicle,
};
use ompas_structs::sym_table::r#ref::RefSymTable;
use ompas_structs::sym_table::VarId;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::env::set_current_dir;
use std::fmt::Display;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::time::SystemTime;

pub mod chronicle;
pub mod flow;

const DEBUG_CHRONICLE: bool = false;

pub async fn convert(
    lv: &LValue,
    env: &LEnv,
    st: RefSymTable,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let time = SystemTime::now();
    let pp_lv = pre_processing(lv, env).await?;

    //let sym_table = RefCell::new(SymTable::default());

    let mut graph = FlowGraph::new(st.clone());

    let flow = convert_lv(&pp_lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_method(None, &mut graph, &flow, env)?;
    //let mut ch = ChronicleTemplate::new("debug", ChronicleKind::Method, st);
    post_processing(&mut ch, env.clone())?;
    graph.flat_bindings();
    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = pp_lv;
    ch.debug.lvalue = lv.clone();
    ch.debug.convert_time = time.elapsed().unwrap();

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

    //println!("Start task declaration.");
    for task in cc.domain.get_tasks().values() {
        //println!("Declaring task: {}", task.get_label());
        tasks.push(declare_task(task, st.clone()));
    }
    //println!("End task declaration.");

    //println!("Start command declaration.");
    for command in cc.domain.get_commands().values() {
        //evaluate the lambda sim.
        //println!("Converting command {}", command.get_label());
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

    //println!("End command declaration.");
    //println!("Start method declaration.");
    //Add all methods to the domain
    for method in cc.domain.get_methods().values() {
        //println!("Converting method {}", method.get_label());

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
    //println!("End method declaration.");

    Ok(PlanningDomain {
        sf,
        tasks,
        methods,
        commands,
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
    let time = SystemTime::now();

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
    ch.debug.lvalue = lv.clone();

    let lv = pre_processing(lv, &cc.env).await?;

    let mut graph = FlowGraph::new(st);

    let flow = convert_lv(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_method(Some(ch), &mut graph, &flow, &cc.env)?;
    post_processing(&mut ch, cc.env.clone())?;

    graph.flat_bindings();
    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = lv;
    ch.debug.convert_time = time.elapsed().unwrap();

    if DEBUG_CHRONICLE {
        debug_with_markdown(label.to_string().as_str(), &ch, "/tmp".into(), true);
    }

    Ok(ch)
}

pub fn declare_task(task: &Task, st: RefSymTable) -> TaskChronicle {
    let task_label_id = st
        .get_sym_id(task.get_label())
        .expect("symbol of task should be defined");

    let mut task_lit: Vec<VarId> = vec![task_label_id];

    for (param, t) in task.get_parameters().inner() {
        task_lit.push(st.new_parameter(param, t.get_domain().clone()))
    }
    TaskChronicle {
        task: task.clone(),
        convert: task_lit,
    }
}

pub fn debug_with_markdown(label: &str, ch: &ChronicleTemplate, path: PathBuf, view: bool) {
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("graph-flow-output_{}", string_date));
    fs::create_dir_all(&path).unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = format!("{}.dot", label);
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.debug.flow_graph.export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let flow_file_name = format!("{}.png", label);
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &flow_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = "lattice.dot";
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.st.export_lattice_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let lattice_file_name = "lattice.png";
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &lattice_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut md_path = path.clone();
    let md_file_name = format!("{}-output.md", label);
    md_path.push(&md_file_name);
    let mut md_file = File::create(&md_path).unwrap();
    let md: String = format!(
        "# Conversion of expression : {}\n
    Time to convert: {}µs\n
    \n

## Chronicles
```
{}
```

## Scheme code

```lisp\n
{}
```
\n
## Post processed Scheme code
```lisp\n
{}
```
## Graph
\n
![]({})
\n


## Type Lattice
\n
![]({})
\n

## Sym Table
```
{}
```
    ",
        label,
        ch.debug.convert_time.as_micros(),
        ch,
        ch.debug.lvalue.format(0),
        ch.debug.post_processed_lvalue.format(0),
        flow_file_name,
        lattice_file_name,
        ch.st
    );

    md_file.write_all(md.as_bytes()).unwrap();

    if view {
        Command::new("google-chrome")
            .arg(&md_file_name)
            .spawn()
            .unwrap();
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
