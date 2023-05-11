use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::planning::conversion::context::ConversionContext;
use crate::planning::conversion::convert_acting_domain;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::{PConfig, PLEnv};
use crate::planning::conversion::flow_graph::algo::p_eval::{p_eval, P_EVAL};
use crate::planning::planner::problem::PlanningDomain;
use crate::planning::planner::solver::PMetric;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::monitor::control::MOD_CONTROL;
use ompas_language::monitor::debug_conversion::{
    ANNOTATE_TASK, CONVERT_DOMAIN, DOC_ANNOTATE_TASK, DOC_CONVERT_DOMAIN, DOC_MOD_DEBUG_CONVERSION,
    DOC_PLAN_TASK, DOC_PLAN_TASK_OPT, DOC_PRE_EVAL_EXPR, DOC_PRE_EVAL_TASK, MOD_DEBUG_CONVERSION,
    PLAN_TASK, PLAN_TASK_OPT, PRE_EVAL_EXPR, PRE_EVAL_TASK,
};
use ompas_language::monitor::model::MOD_DOMAIN;
use ompas_middleware::logger::LogClient;
use sompas_core::expand;
use sompas_language::LOG_TOPIC_INTERPRETER;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::time::SystemTime;
use tokio::sync::broadcast;

#[derive(Default)]
pub struct ModDebugConversion {}

impl From<ModDebugConversion> for LModule {
    fn from(m: ModDebugConversion) -> Self {
        let mut m = LModule::new(m, MOD_DEBUG_CONVERSION, DOC_MOD_DEBUG_CONVERSION);
        m.add_async_fn(CONVERT_DOMAIN, convert_domain, DOC_CONVERT_DOMAIN, false);
        m.add_async_fn(PLAN_TASK, plan_task, DOC_PLAN_TASK, false);
        m.add_async_fn(PLAN_TASK_OPT, plan_task_opt, DOC_PLAN_TASK_OPT, false);
        m.add_async_fn(PRE_EVAL_TASK, pre_eval_task, DOC_PRE_EVAL_TASK, false);
        m.add_async_fn(PRE_EVAL_EXPR, pre_eval_expr, DOC_PRE_EVAL_EXPR, false);
        m.add_async_fn(ANNOTATE_TASK, annotate_task, DOC_ANNOTATE_TASK, false);
        m
    }
}

#[async_scheme_fn]
pub async fn convert_domain(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_DOMAIN)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let time = SystemTime::now();
    let pd: PlanningDomain = convert_acting_domain(&context).await?;
    let time = time.elapsed().expect("could not get time").as_micros();
    Ok(format!("{}\n\nTime to convert: {} µs.", pd, time))
}

async fn _plan_task(env: &LEnv, args: &[LValue], opt: bool) -> LResult {
    let task: LValue = args.into();
    println!("task to plan: {}", task);
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let ctx = env.get_context::<ModModel>(MOD_DOMAIN)?;
    //let mut context: ConversionContext = ctx.get_conversion_context().await;
    let mut env: LEnv = ctx.get_plan_env().await;
    let state = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state));

    acting_manager
        .start_continuous_planning(env, if opt { Some(PMetric::Makespan) } else { None })
        .await;

    let debug = LValue::from(args).to_string();
    let args = args.iter().map(|lv| lv.as_cst().unwrap()).collect();

    let _pr = acting_manager.new_high_level_task(debug, args).await;
    /*
    let actions = vec![PAction {
        args: args
            .iter()
            .map(|lv| ActionParam::Instantiated(lv.try_into().unwrap()))
            .collect(),
        origin: ChronicleOrigin::Refinement {
            instance_id: 0,
            task_id: 0,
        },
        pr: ProcessRef::Relative(0, vec![Label::Action(0)]),
    }];

    let mut pp: PlanningProblem = finite_problem(actions, &context).await?;

    /*for instance in &pp.instance.instances {
        print!("{}", instance.om.chronicle.as_ref().unwrap())
    }*/

    let mut table = ActingVarRefTable::default();

    let aries_problem: chronicles::Problem = encode_chronicles(&mut table, &pp).await?;
    /*let model = &aries_problem.context.model;

    for instance in &aries_problem.chronicles {
        let presence = instance.chronicle.presence;
        let origin = instance.origin;
        println!(
            "presence of {:?}: {:?}",
            origin,
            model.state.value(presence)
        );
    }*/

    let result = run_solver(
        aries_problem,
        match opt {
            true => Some(planner::solver::PMetric::Makespan),
            false => None,
        },
    );
    //println!("{}", format_partial_plan(&pb, &x)?);

    let result: LValue = if let Ok(Some(pr)) = result {
        //result::print_chronicles(&pr);
        let solved: Vec<ChronicleInstance> =
            instance::instantiate_chronicles(&pp, &pr, &mut table).await;
        /*for instance in &solved {
            print!("{}", instance.om.chronicle.as_ref().unwrap())
        }*/

        let raw_plan = acting::extract_raw_plan(&solved);
        println!("RAW PLAN:\n{}", raw_plan);

        //println!("len = {}", pp.instance.instances.len());
        // Filter instances to keep only those that are present in the solution, removing chronicles of unused methods
        let instances: Vec<_> = pp
            .instance
            .instances
            .drain(..)
            .filter(|c| {
                let presence = c.am.chronicle.as_ref().unwrap().get_presence();
                let cst = get_var_as_cst(&table, &pr.ass, &pr.fp.model, presence);
                //println!("{cst}");
                Cst::Bool(true) == cst
            })
            .collect();

        //println!("len = {}", instances.len());

        let plan_result = ActingPlanResult {
            instances,
            table,
            assignements: pr.ass.clone(),
            finite_problem: pr.fp,
        };

        //acting_manager.absorb_plan_result(plan_result).await;

        /*let plan = result::extract_plan(x);
        println!("plan:\n{}\n{}", plan.format(), plan.format_hierarchy());*/
        //let first_task_id = plan.get_first_subtask().unwrap();
        /*let subplan = plan.extract_sub_plan(first_task_id);
        println!(
            "subplan: \n{}\n{}",
            subplan.format(),
            subplan.format_hierarchy()
        );*/
        //result::extract_instantiated_methods(x)?
        LValue::Nil
    } else {
        string!("no solution found".to_string())
    };

    Ok(result)*/
    let mut recv: broadcast::Receiver<bool> =
        acting_manager.subscribe_on_plan_update().await.unwrap();
    recv.recv()
        .await
        .expect("Error while waiting on plan update.");
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn plan_task(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_task(env, args, false).await
}

#[async_scheme_fn]
pub async fn plan_task_opt(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_task(env, args, true).await
}

#[async_scheme_fn]
pub async fn pre_eval_task(env: &LEnv, task: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_DOMAIN)?;
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
    let ctx = env.get_context::<ModModel>(MOD_DOMAIN)?;
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
    let ctx = env.get_context::<ModModel>(MOD_DOMAIN)?;
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
