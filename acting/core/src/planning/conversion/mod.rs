use crate::model::acting_domain::acting_model_collection::ActingModelCollection;
use crate::model::acting_domain::model::ActingModel;
use crate::model::chronicle::Chronicle;
#[cfg(feature = "conversion_data")]
use crate::model::chronicle::ChronicleDebugData;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::scheme::exec::ModExec;
use crate::planning::conversion::chronicle::convert_graph;
use crate::planning::conversion::chronicle::post_processing::try_eval_apply;
use crate::planning::conversion::context::ConversionContext;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::convert_lv;
use crate::planning::conversion::flow_graph::algo::p_eval::p_eval;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use crate::planning::conversion::flow_graph::algo::post_processing::flow_graph_post_processing;
use crate::planning::conversion::flow_graph::algo::pre_processing::pre_processing;
use crate::planning::conversion::flow_graph::define_table::DefineTable;
use crate::planning::conversion::flow_graph::graph::FlowGraph;
use crate::planning::planner::problem::PlanningDomain;
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG};
#[allow(unused)]
use debug_print::debug_println;
use env_param::EnvParam;
use ompas_language::exec::MOD_EXEC;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
#[cfg(feature = "conversion_data")]
use std::env::set_current_dir;
#[cfg(feature = "conversion_data")]
use std::fmt::Write as OtherWrite;
#[cfg(feature = "conversion_data")]
use std::fs;
#[cfg(feature = "conversion_data")]
use std::fs::{File, OpenOptions};
#[cfg(feature = "conversion_data")]
use std::io::Write;
#[cfg(feature = "conversion_data")]
use std::path::PathBuf;
#[cfg(feature = "conversion_data")]
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::SystemTime;
use tokio::runtime::Handle;

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
    mut p_env: PLEnv,
    st: RefSymTable,
) -> Result<ActingModel, LRuntimeError> {
    let handle = Handle::current();
    let p_eval_lv = p_eval(lv, &mut p_env).await?;
    let r: Result<_, LRuntimeError> = handle
        .spawn_blocking(move || {
            let lv_om = annotate(p_eval_lv);

            let pp_lv = pre_processing(&lv_om, &p_env)?;

            let chronicle = match _convert(ch.clone(), &pp_lv, &mut p_env, st) {
                Ok(ch) => Some(ch),
                Err(e) => {
                    println!("{}", e);
                    None
                }
            };
            Ok((lv_om, pp_lv, chronicle, p_env))
        })
        .await
        .unwrap();

    let (lv_om, pp_lv, mut chronicle, p_env) = r?;

    if let Some(ch) = &mut chronicle {
        try_eval_apply(ch, &p_env.env).await?;
        #[cfg(feature = "conversion_data")]
        try_eval_apply(
            &mut ch.meta_data.debug.as_mut().unwrap().raw_chronicle,
            &p_env.env,
        )
        .await?;
    }

    Ok(ActingModel {
        lv: lv.clone(),
        lv_om,
        lv_expanded: Some(pp_lv),
        runtime_info: Default::default(),
        chronicle,
    })
}

pub fn _convert(
    ch: Option<Chronicle>,
    lv: &LValue,
    p_env: &mut PLEnv,
    st: RefSymTable,
) -> Result<Chronicle, LRuntimeError> {
    let time = SystemTime::now();
    let n_conversion = N_CONVERSION.fetch_add(1, Ordering::Relaxed);
    let mut define_table: DefineTable = Default::default();
    if let Some(ch) = &ch {
        for param in &ch.get_name()[1..] {
            let sym = st.get_symbol(*param);
            //if sym == "?r" {
            //println!("{sym}");
            //}
            define_table.insert(sym, *param);
        }
    }
    let mut graph = FlowGraph::new(st);

    if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
        println!(
            "({} ms) conversion n°{n_conversion}:\n{}",
            time.elapsed().unwrap().as_millis(),
            lv.format(0)
        );
    }

    let flow = convert_lv(lv, &mut graph, &mut define_table)?;
    graph.flow = flow;

    #[cfg(feature = "conversion_data")]
    let raw_flow_graph = graph.clone();

    flow_graph_post_processing(&mut graph)?;
    if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
        println!(
            "({} ms) flow_graph_post_processing({n_conversion}) = ok!",
            time.elapsed().unwrap().as_millis()
        );
    }
    let max_capacity = match p_env.env.get_context::<ModExec>(MOD_EXEC) {
        Ok(m) => m.acting_manager.resource_manager.get_max_capacity() as i64,
        Err(_) => 1,
    };

    let cv = &ConvertParameters { max_capacity };

    #[cfg(feature = "conversion_data")]
    let mut ch = convert_graph(ch, &mut graph, flow, &p_env.env, cv)?;
    #[cfg(not(feature = "conversion_data"))]
    let ch = convert_graph(ch, &mut graph, flow, &p_env.env, cv)?;

    if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
        println!(
            "({} ms) chronicle: {}",
            time.elapsed().unwrap().as_millis(),
            ch
        );
    }
    graph.flat_bindings();
    #[cfg(feature = "conversion_data")]
    {
        let raw_chronicle = ch.clone();

        let debug = ChronicleDebugData {
            raw_flow_graph,
            flow_graph: graph,
            raw_chronicle: Box::new(raw_chronicle),
            convert_time: time.elapsed().unwrap(),
        };
        ch.meta_data.debug = Some(debug);
    }
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

    let ConversionContext {
        state,
        domain,
        st,
        env,
    } = cc;

    //add new types to list of types.
    //panic!("for no fucking reason");
    //Add actions, tasks and methods symbols to ch.sym_table:

    //Add tasks to domain

    let mut tasks_list = vec![];
    let mut commands_list = vec![];
    let mut methods_list = vec![];
    let mut templates = vec![];
    let sf = cc.domain.get_state_functions().values().cloned().collect();

    //println!("Start task declaration.");
    for label in cc.domain.get_tasks().keys() {
        //println!("Declaring task: {}", task.get_label());
        tasks_list.push(label.to_string());
    }

    for label in cc.domain.get_commands().keys() {
        commands_list.push(label.to_string())
    }

    for label in cc.domain.get_methods().keys() {
        methods_list.push(label.to_string())
    }

    let mut acting_model_collection = ActingModelCollection::default();
    acting_model_collection
        .pre_compute_acting_models(domain, env, state.clone(), st)
        .await;

    let ActingModelCollection {
        tasks,
        methods,
        commands,
    } = acting_model_collection;

    for (_, am) in tasks {
        templates.push(am)
    }

    for (_, am) in methods {
        templates.push(am)
    }

    for (_, am) in commands {
        templates.push(am)
    }

    Ok(PlanningDomain {
        sf,
        tasks: tasks_list,
        methods: methods_list,
        commands: commands_list,
        templates,
        st: st.clone(),
    })
}
#[cfg(feature = "conversion_data")]
pub fn debug_with_markdown(label: &str, am: &ActingModel, path: PathBuf, view: bool) {
    let raw_ch = am.chronicle.as_ref().unwrap();
    let ch = am.get_clean_instantiated_chronicle().unwrap();
    let label = label.replace('/', "_");
    let mut path = path;
    path.push(format!("graph-flow-output_{}", Master::get_string_date()));
    fs::create_dir_all(&path).unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = format!("raw_{}.dot", label);
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let raw_flow_graph = &raw_ch.meta_data.debug.as_ref().unwrap().raw_flow_graph;
    let dot = raw_flow_graph.export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();

    let raw_flow_file_name = format!("raw_{}.png", label);
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &raw_flow_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = format!("{}.dot", label);
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let flow_graph = &raw_ch.meta_data.debug.as_ref().unwrap().flow_graph;
    let dot = flow_graph.export_dot();
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
    let mut md_file = OpenOptions::new()
        .create(true)
        .write(true)
        .open(&md_path)
        .unwrap();

    let mut md = format!(
        "# Conversion of expression : {}\n
    Time to convert: {}µs\n
    \n",
        label,
        raw_ch
            .meta_data
            .debug
            .as_ref()
            .unwrap()
            .convert_time
            .as_micros(),
    );
    let raw_chronicle = raw_ch
        .meta_data
        .debug
        .as_ref()
        .unwrap()
        .raw_chronicle
        .clone()
        .add_models(vec![])
        .instantiate(vec![]);

    writeln!(md, "## Raw chronicle\n```\n{}\n```", raw_chronicle).unwrap();

    writeln!(md, " ## Post processed chronicle\n```\n{}``̀`\n", ch).unwrap();
    writeln!(md, "## Chronicle post-processing information:\n").unwrap();
    writeln!(
        md,
        "- Variables: {} -> {}",
        raw_chronicle.variables.len(),
        ch.variables.len()
    )
    .unwrap();
    writeln!(
        md,
        "- Constraints: {} -> {}",
        raw_chronicle.constraints.len(),
        ch.variables.len()
    )
    .unwrap();
    writeln!(
        md,
        "- Conditions: {} -> {}",
        raw_chronicle.conditions.len(),
        ch.conditions.len()
    )
    .unwrap();

    writeln!(md, "## Scheme code\n```lisp\n{}\n```", am.lv.format(0),).unwrap();

    writeln!(
        md,
        "## Post processed Scheme code\n```lisp\n{}\n```",
        am.lv_expanded.as_ref().unwrap().format(0),
    )
    .unwrap();
    writeln!(md, "## Raw flow graph\n![]({})\n", raw_flow_file_name,).unwrap();
    writeln!(
        md,
        "## Post processed flow graph\n![]({})\n",
        flow_file_name,
    )
    .unwrap();

    writeln!(md, "## Flow graph post-processing information:\n").unwrap();
    writeln!(
        md,
        "- Nodes:{} -> {}",
        raw_flow_graph.n_nodes(raw_flow_graph.flow),
        flow_graph.n_nodes(flow_graph.flow),
    )
    .unwrap();
    writeln!(
        md,
        "- Branching: {} -> {}",
        raw_flow_graph.n_branching(raw_flow_graph.flow),
        flow_graph.n_branching(flow_graph.flow),
    )
    .unwrap();

    writeln!(md, "## Type Lattice\n![]({})", lattice_file_name,).unwrap();

    writeln!(md, "## Sym Table\n```\n{}\n```", ch.st).unwrap();

    md_file.write_all(md.as_bytes()).unwrap();

    if view {
        Command::new("google-chrome")
            .arg(&md_file_name)
            .spawn()
            .unwrap();
    }
}
