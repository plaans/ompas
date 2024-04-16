use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::planning::conversion::context::ConversionContext;
use crate::planning::conversion::convert_acting_domain;
#[cfg(feature = "conversion_data")]
use crate::planning::conversion::debug_with_markdown;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::{PConfig, PLEnv};
use crate::planning::conversion::flow_graph::algo::p_eval::{p_eval, P_EVAL};
use crate::planning::planner::problem::PlanningDomain;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::monitor::debug_conversion::*;
use ompas_language::monitor::model::MOD_MODEL;
use ompas_middleware::logger::LogClient;
use ompas_middleware::Master;
use sompas_core::expand;
use sompas_language::LOG_TOPIC_INTERPRETER;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::time::SystemTime;

#[derive(Default)]
pub struct ModDebugConversion {}

impl From<ModDebugConversion> for LModule {
    fn from(m: ModDebugConversion) -> Self {
        let mut m = LModule::new(m, MOD_DEBUG_CONVERSION, DOC_MOD_DEBUG_CONVERSION);
        m.add_async_fn(CONVERT_DOMAIN, convert_domain, DOC_CONVERT_DOMAIN, false);
        m.add_async_fn(
            EXPORT_TYPE_LATTICE,
            export_type_lattice,
            DOC_EXPORT_TYPE_LATTICE,
            false,
        );
        m.add_async_fn(PRE_EVAL_TASK, pre_eval_task, DOC_PRE_EVAL_TASK, false);
        m.add_async_fn(PRE_EVAL_EXPR, pre_eval_expr, DOC_PRE_EVAL_EXPR, false);
        m.add_async_fn(ANNOTATE_TASK, annotate_task, DOC_ANNOTATE_TASK, false);
        m.add_async_fn(TRANSLATE, translate, DOC_TRANSLATE, false);
        m
    }
}

#[async_scheme_fn]
pub async fn translate(_env: &LEnv, _obj: String) -> Result<(), LRuntimeError> {
    #[cfg(feature = "conversion_data")]
    {
        let ctx = _env.get_context::<ModModel>(MOD_MODEL)?;
        let context: ConversionContext = ctx.get_conversion_context().await;
        let pd: PlanningDomain = convert_acting_domain(&context).await?;
        debug_with_markdown(
            &_obj,
            pd.templates
                .iter()
                .find(|am| {
                    am.chronicle
                        .as_ref()
                        .unwrap()
                        .get_label()
                        .unwrap()
                        .contains(&_obj)
                })
                .unwrap(),
            "/tmp/".into(),
            true,
        );
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn convert_domain(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let time = SystemTime::now();
    let pd: PlanningDomain = convert_acting_domain(&context).await?;
    let time = time.elapsed().expect("could not get time").as_micros();
    Ok(format!("{}\n\nTime to convert: {} µs.", pd, time))
}

#[async_scheme_fn]
pub async fn export_type_lattice(env: &LEnv) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let ctx: ConversionContext = ctx.get_conversion_context().await;

    let mut path: PathBuf = Master::get_run_dir();
    path.push("lattice_of_type");
    path.push(format!("lattice_of_type_{}", Master::get_string_date()));
    fs::create_dir_all(&path).unwrap();
    let mut path_dot = path.clone();
    let dot_file_name = "lattice.dot";
    path_dot.push(dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ctx.st.get_lattice().export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let graph_file_name = "lattice.png";
    Command::new("dot")
        .args(["-Tpng", dot_file_name, "-o", graph_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    let mut md_path = path.clone();
    let md_file_name = "lattice.md";
    md_path.push(md_file_name);
    let mut md_file = File::create(&md_path).unwrap();
    let md: String = format!(
        "# Type Network : \n
![]({})
    ",
        graph_file_name,
    );

    md_file.write_all(md.as_bytes()).unwrap();

    Command::new("google-chrome")
        .arg(md_file_name)
        .spawn()
        .unwrap();

    Ok(())
}

#[async_scheme_fn]
pub async fn pre_eval_task(env: &LEnv, task: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let mut context: ConversionContext = ctx.get_conversion_context().await;
    context
        .env
        .update_context(ModState::new_from_snapshot(context.state.clone()));

    let t = context
        .domain
        .tasks
        .get(task[0].to_string().as_str())
        .unwrap();

    let params = t.get_parameters().get_labels();
    let mut pc = PConfig::default();
    pc.avoid.insert(EXEC_TASK.to_string());
    //pc.avoid.insert(CHECK.to_string());
    assert_eq!(params.len(), task.len() - 1);
    for (param, value) in params.iter().zip(task[1..].iter()) {
        pc.p_table
            .add_instantiated(param.to_string(), value.clone());
    }

    for m_label in t.get_methods() {
        let mut pc = pc.clone();
        let method = context.domain.methods.get(m_label).unwrap();
        for param in &method.parameters.get_labels()[params.len()..] {
            pc.p_table.add_param(param.to_string());
        }
        let body = method.get_body();
        let mut env = context.env.clone();
        env.log = LogClient::new(P_EVAL, LOG_TOPIC_INTERPRETER).await;
        let lambda: LLambda = body.try_into()?;
        let lv = lambda.get_body();
        let mut p_env = PLEnv {
            env,
            unpure_bindings: Default::default(),
            pc,
        };
        let plv = p_eval(lv, &mut p_env).await?;
        println!(
            "Pre eval method {m_label} of task {}:\n{}\n->\n{}",
            LValue::from(task).format(0),
            lv.format(0),
            plv.format(0),
        )
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn annotate_task(env: &LEnv, task: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let mut context: ConversionContext = ctx.get_conversion_context().await;
    context
        .env
        .update_context(ModState::new_from_snapshot(context.state.clone()));

    let t = context
        .domain
        .tasks
        .get(task[0].to_string().as_str())
        .unwrap();

    let params = t.get_parameters().get_labels();
    let mut pc = PConfig::default();
    pc.avoid.insert(EXEC_TASK.to_string());
    //pc.avoid.insert(CHECK.to_string());
    assert_eq!(params.len(), task.len() - 1);
    for (param, value) in params.iter().zip(task[1..].iter()) {
        pc.p_table
            .add_instantiated(param.to_string(), value.clone());
    }

    for m_label in t.get_methods() {
        let mut pc = pc.clone();
        let method = context.domain.methods.get(m_label).unwrap();
        for param in &method.parameters.get_labels()[params.len()..] {
            pc.p_table.add_param(param.to_string());
        }
        let body = method.get_body();
        let mut env = context.env.clone();
        env.log = LogClient::new(P_EVAL, LOG_TOPIC_INTERPRETER).await;
        let lambda: LLambda = body.try_into()?;
        let lv = lambda.get_body();
        let mut p_env = PLEnv {
            env,
            unpure_bindings: Default::default(),
            pc,
        };
        let lv = p_eval(lv, &mut p_env).await?;
        let lv = annotate(lv);
        println!(
            "annotate(p_eval({m_label}/task({}))):\n{}\n->\n{}",
            LValue::from(task).format(0),
            lv.format(0),
            lv.format(0),
        )
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn pre_eval_expr(env: &LEnv, lv: LValue) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let mut context: ConversionContext = ctx.get_conversion_context().await;
    context
        .env
        .update_context(ModState::new_from_snapshot(context.state.clone()));

    let mut pc = PConfig::default();
    pc.avoid.insert(EXEC_TASK.to_string());
    //pc.avoid.insert(CHECK.to_string());
    let mut env = context.env.clone();
    let mut p_env = PLEnv {
        env: context.env.clone(),
        unpure_bindings: Default::default(),
        pc,
    };
    env.log = LogClient::new(P_EVAL, LOG_TOPIC_INTERPRETER).await;
    let plv: LValue = expand(&lv, true, &mut p_env.env).await?;
    let plv: LValue = p_eval(&plv, &mut p_env).await?;
    println!("Pre eval expr:\n{}\n->\n{}", lv.format(0), plv.format(0),);

    Ok(())
}
