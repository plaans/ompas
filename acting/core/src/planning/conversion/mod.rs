use crate::model::acting_domain::model::{ActingModel, ModelKind};
use crate::model::acting_domain::parameters::Parameters;
use crate::model::acting_domain::task::Task;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::process_ref::{MethodLabel, RefinementLabel};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::scheme::exec::ModExec;
use crate::planning::conversion::chronicle::convert_graph;
use crate::planning::conversion::chronicle::post_processing::post_processing;
use crate::planning::conversion::context::ConversionContext;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::convert_lv;
use crate::planning::conversion::flow_graph::algo::p_eval::p_eval;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::{PConfig, PLEnv};
use crate::planning::conversion::flow_graph::algo::post_processing::flow_graph_post_processing;
use crate::planning::conversion::flow_graph::algo::pre_processing::pre_processing;
use crate::planning::conversion::flow_graph::graph::FlowGraph;
use crate::planning::planner::problem::{
    ChronicleInstance, PlanningDomain, PlanningInstance, PlanningProblem,
};
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG_ON};
use aries_planning::chronicles::{ChronicleOrigin, TaskId};
use chrono::{DateTime, Utc};
#[allow(unused)]
use debug_print::debug_println;
use env_param::EnvParam;
use ompas_language::exec::MOD_EXEC;
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
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::SystemTime;

pub mod chronicle;
pub mod context;
pub mod flow_graph;
pub mod point_algebra;

#[allow(dead_code)]
static N_CONVERSION: AtomicU32 = AtomicU32::new(0);
const MAX_QUANTITY_VALUE: i64 = 1000;
pub static TEST_CONVERSION: EnvParam<bool> = EnvParam::new("OMPAS_TEST_CONVERSION", "false");

pub struct ConvertParameters {
    pub max_capacity: i64,
}

impl Default for ConvertParameters {
    fn default() -> Self {
        Self {
            max_capacity: MAX_QUANTITY_VALUE,
        }
    }
}

pub async fn convert(
    ch: Option<Chronicle>,
    lv: &LValue,
    p_env: &mut PLEnv,
    st: RefSymTable,
) -> Result<ActingModel, LRuntimeError> {
    let p_eval_lv = p_eval(lv, p_env).await?;
    //debug_println!("{}\np_eval =>\n{}", lv.format(0), p_eval_lv.format(0));
    let lv_om = annotate(p_eval_lv);
    //debug_println!("annotate =>\n{}", lv_om.format(0));

    let pp_lv = pre_processing(&lv_om, p_env).await?;
    //debug_println!("pre_processing =>\n{}", pp_lv.format(0));

    let chronicle = match _convert(ch, &pp_lv, p_env, st).await {
        Ok(ch) => Some(ch),
        Err(e) => {
            println!("{}", e);
            None
        }
    };

    Ok(ActingModel {
        lv: lv.clone(),
        lv_om,
        lv_expanded: Some(pp_lv),
        instantiations: vec![],
        chronicle,
    })
}

pub async fn _convert(
    ch: Option<Chronicle>,
    lv: &LValue,
    p_env: &mut PLEnv,
    st: RefSymTable,
) -> Result<Chronicle, LRuntimeError> {
    let n_conversion = N_CONVERSION.fetch_add(1, Ordering::Relaxed);
    let mut graph = FlowGraph::new(st);

    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::Full {
        println!("conversion n°{n_conversion}:\n{}", lv.format(0));
    }
    let flow = convert_lv(lv, &mut graph, &mut Default::default())?;
    let time = SystemTime::now();
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::Full {
        println!("flow_graph_post_processing({n_conversion}) = ok!");
    }*/
    let max_capacity = match p_env.env.get_context::<ModExec>(MOD_EXEC) {
        Ok(m) => m.acting_manager.resource_manager.get_max_capacity() as i64,
        Err(_) => 1,
    };

    /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::Full {
        println!("_convert/max_capacity = {}", max_capacity);
    }*/

    let cv = &ConvertParameters { max_capacity };

    let mut ch = convert_graph(ch, &mut graph, &flow, &p_env.env, cv)?;

    post_processing(&mut ch, &p_env.env).await?;
    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::Full {
        println!("chronicle: {}", ch);
    }
    graph.flat_bindings();
    ch.meta_data.flow_graph = graph;
    ch.meta_data.convert_time = time.elapsed().unwrap();
    Ok(ch)
}

pub async fn p_convert_task(
    task: &[LValue],
    context: &ConversionContext,
) -> Result<PlanningProblem, LRuntimeError> {
    let t = context
        .domain
        .tasks
        .get(task[0].to_string().as_str())
        .unwrap();

    let params = t.get_parameters().get_labels();
    let mut pc = PConfig::default();
    assert_eq!(params.len(), task.len() - 1);
    for (param, value) in params.iter().zip(task[1..].iter()) {
        pc.p_table
            .add_instantiated(param.to_string(), value.clone());
    }

    let mut instances = vec![];
    let mut methods = vec![];
    match t.get_model(&ModelKind::PlanModel) {
        Some(model) => {
            let method_lambda: LLambda = model.try_into().expect("");

            let om: ActingModel = convert_abstract_task_to_chronicle(
                &method_lambda,
                t.get_label(),
                None,
                t.get_parameters(),
                context,
                ChronicleKind::Method,
                pc.clone(),
            )
            .await?;

            instances.push(ChronicleInstance {
                generated: true,
                origin: ChronicleOrigin::Refinement {
                    refined: vec![TaskId {
                        instance_id: 0,
                        task_id: 0,
                    }],
                    template_id: 0,
                },
                am: om,
                pr: Default::default(),
                refinement_label: RefinementLabel {
                    refinement_id: 0,
                    method_label: MethodLabel::Possibility(0),
                },
            });
        }
        None => {
            for (i, m_label) in t.get_methods().iter().enumerate() {
                methods.push(m_label.to_string());
                let mut pc = pc.clone();
                let method = context.domain.methods.get(m_label).unwrap();
                for param in &method.parameters.get_labels()[params.len()..] {
                    pc.p_table.add_param(param.to_string());
                }

                let method_lambda: LLambda = method.get_body().try_into().expect("");

                let om: ActingModel = convert_abstract_task_to_chronicle(
                    &method_lambda,
                    &method.label,
                    Some(t),
                    method.get_parameters(),
                    context,
                    ChronicleKind::Method,
                    pc.clone(),
                )
                .await?;

                instances.push(ChronicleInstance {
                    generated: true,
                    origin: ChronicleOrigin::Refinement {
                        refined: vec![TaskId {
                            instance_id: 0,
                            task_id: 0,
                        }],
                        template_id: 0,
                    },
                    am: om,
                    pr: Default::default(),
                    refinement_label: RefinementLabel {
                        refinement_id: 0,
                        method_label: MethodLabel::Possibility(i),
                    },
                });
            }
        }
    }

    Ok(PlanningProblem {
        domain: PlanningDomain {
            sf: vec![],
            methods,
            tasks: vec![t.get_label().clone()],
            commands: vec![],
            templates: vec![],
            st: context.st.clone(),
        },
        instance: PlanningInstance {
            state: context.state.clone(),
            tasks: vec![LValue::from(task).try_into()?],
            instances,
        },
        st: context.st.clone(),
    })
}

#[allow(unused)]
const CONVERT_LVALUE_TO_CHRONICLE: &str = "convert_lvalue_to_chronicle";
#[allow(unused)]
const CONVERT_DOMAIN_TO_CHRONICLE_HIERARCHY: &str = "convert_domain_to_chronicle_hierarchy";

pub async fn convert_acting_domain(
    cc: &ConversionContext,
) -> lruntimeerror::Result<PlanningDomain> {
    //for each action: translate to chronicle

    let pc = PConfig::default();

    let st = cc.st.clone();

    //add new types to list of types.
    //panic!("for no fucking reason");
    //Add actions, tasks and methods symbols to ch.sym_table:

    //Add tasks to domain

    let mut tasks = vec![];
    let mut commands = vec![];
    let mut methods = vec![];
    let mut templates = vec![];
    let sf = cc.domain.get_state_functions().values().cloned().collect();

    //println!("Start task declaration.");
    for task in cc.domain.get_tasks().values() {
        //println!("Declaring task: {}", task.get_label());
        tasks.push(task.get_label().to_string());
    }
    //println!("End task declaration.");

    //println!("Start command declaration.");
    for command in cc.domain.get_commands().values() {
        let mut pc = pc.clone();

        for param in &command.get_parameters().get_labels() {
            pc.p_table.add_param(param.to_string());
        }
        //evaluate the lambda sim.
        //println!("Converting command {}", command.get_label());
        let template = convert_abstract_task_to_chronicle(
            &command
                .get_model(&ModelKind::PlanModel)
                .unwrap()
                .try_into()?,
            command.get_label(),
            None,
            command.get_parameters(),
            cc,
            ChronicleKind::Command,
            pc,
        )
        .await?;

        templates.push(template);
        commands.push(command.get_label().to_string());
    }

    //println!("End command declaration.");
    //println!("Start method declaration.");
    //Add all methods to the domain
    for method in cc.domain.get_methods().values() {
        //println!("Converting method {}", method.get_label());

        let mut pc = pc.clone();

        for param in &method.parameters.get_labels() {
            pc.p_table.add_param(param.to_string());
        }

        let task = cc.domain.get_tasks().get(method.get_task_label()).unwrap();

        let method_lambda: LLambda = method.get_body().try_into().expect("");

        let template = convert_abstract_task_to_chronicle(
            &method_lambda,
            &method.label,
            Some(task),
            method.get_parameters(),
            cc,
            ChronicleKind::Method,
            pc,
        )
        .await?;

        templates.push(template);
        methods.push(method.get_label().to_string());
    }
    //println!("End method declaration.");

    Ok(PlanningDomain {
        sf,
        tasks,
        methods,
        commands,
        templates,
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
    pc: PConfig,
) -> lruntimeerror::Result<ActingModel> {
    let st = cc.st.clone();

    let symbol_id = st.get_sym_id(&label.to_string()).unwrap();

    let mut ch = Chronicle::new(label.to_string(), kind, st.clone());
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

    let mut p_env = PLEnv {
        env: cc.env.clone(),
        unpure_bindings: Default::default(),
        pc: pc.clone(),
    };

    let om = convert(Some(ch), lv, &mut p_env, st).await?;

    /*let lv = p_eval(lv, &mut p_env).await?;
    //println!("lv: {}", lv.format(4));
    //panic!();
    let lv = pre_processing(&lv, &cc.env).await?;

    let mut graph = FlowGraph::new(st);

    let flow = convert_lv(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    if DEBUG_CHRONICLE && task.is_some() {
        ch.meta_data.flow_graph = graph.clone();
        debug_with_markdown(label.to_string().as_str(), &ch, "/tmp".into(), true);
    }
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_graph(Some(ch), &mut graph, &flow, &cc.env)?;
    post_processing(&mut ch, cc.env.clone())?;

    graph.flat_bindings();
    ch.meta_data.flow_graph = graph;
    ch.meta_data.post_processed_lvalue = lv;
    ch.meta_data.convert_time = time.elapsed().unwrap();

    if DEBUG_CHRONICLE {
        debug_with_markdown(label.to_string().as_str(), &ch, "/tmp".into(), true);
    }*/

    Ok(om)
}

pub fn debug_with_markdown(label: &str, om: &ActingModel, path: PathBuf, view: bool) {
    let ch = om.chronicle.as_ref().unwrap();
    let label = label.replace('/', "_");
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("graph-flow-output_{}", string_date));
    fs::create_dir_all(&path).unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = format!("{}.dot", label);
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.meta_data.flow_graph.export_dot();
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
    path_dot.push(dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.st.export_lattice_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let lattice_file_name = "lattice.png";
    Command::new("dot")
        .args(["-Tpng", dot_file_name, "-o", lattice_file_name])
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
        ch.meta_data.convert_time.as_micros(),
        ch,
        om.lv.format(0),
        om.lv_expanded.as_ref().unwrap().format(0),
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
