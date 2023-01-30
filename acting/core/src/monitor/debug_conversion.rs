use crate::monitor::domain::ModDomain;
use ompas_language::monitor::debug_conversion::{
    CONVERT_DOMAIN, DOC_CONVERT_DOMAIN, DOC_MOD_DEBUG_CONVERSION, DOC_PLAN_TASK,
    MOD_DEBUG_CONVERSION, PLAN_TASK,
};
use ompas_language::monitor::domain::MOD_DOMAIN;
use ompas_planning::aries::solver::run_solver_for_htn;
use ompas_planning::aries::{generate_chronicles, solver};
use ompas_planning::conversion::convert_acting_domain;
use ompas_structs::conversion::context::ConversionContext;
use ompas_structs::planning::domain::PlanningDomain;
use ompas_structs::planning::instance::PlanningInstance;
use ompas_structs::planning::problem::PlanningProblem;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::string;
use std::time::SystemTime;

#[derive(Default)]
pub struct ModDebugConversion {}

impl From<ModDebugConversion> for LModule {
    fn from(m: ModDebugConversion) -> Self {
        let mut m = LModule::new(m, MOD_DEBUG_CONVERSION, DOC_MOD_DEBUG_CONVERSION);
        m.add_async_fn(CONVERT_DOMAIN, convert_domain, DOC_CONVERT_DOMAIN, false);
        m.add_async_fn(PLAN_TASK, plan_task, DOC_PLAN_TASK, false);
        m
    }
}

/*
#[async_scheme_fn]
pub async fn convert_expr(env: &LEnv, expr: &LValue) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DEBUG_CONVERSION).unwrap();
    let mut context: ConversionContext = ctx.get_conversion_context().await;

    let lv = expand(expr, true, &mut context.env).await?;

    let mut ch = ConversionCollection::default();

    let time = SystemTime::now();
    let mut chronicle = ChronicleTemplate::new(&mut ch, "unnamed_chronicle", ChronicleKind::Method);

    let pre_processed = pre_processing(&lv, &context, &mut ch)?;
    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        &context,
        &mut ch,
        MetaData::new(true, false),
    )?;

    chronicle.absorb_expression_chronicle(ec);

    post_processing(&mut chronicle, &context, &mut ch)?;
    let time = time.elapsed().expect("could not get time").as_micros();
    let string = chronicle.format(&ch.sym_table, true);

    Ok(format!("{}\n\n Time to convert: {} µs.", string, time))
}*/

#[async_scheme_fn]
pub async fn convert_domain(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let time = SystemTime::now();
    let pd: PlanningDomain = convert_acting_domain(&context).await?;
    let time = time.elapsed().expect("could not get time").as_micros();
    Ok(format!("{}\n\nTime to convert: {} µs.", pd, time))
}

#[async_scheme_fn]
pub async fn plan_task(env: &LEnv, args: &[LValue]) -> LResult {
    let task: LValue = args.into();
    println!("task to plan: {}", task);
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let pd: PlanningDomain = convert_acting_domain(&context).await?;

    let st = pd.st.clone();
    let problem: PlanningProblem = PlanningProblem {
        domain: pd,
        instance: PlanningInstance {
            state: context.state,
            tasks: vec![task.try_into()?],
        },
        st,
    };

    let mut aries_problem = generate_chronicles(&problem)?;

    //println!("{}", aries_problem)
    let result = run_solver_for_htn(&mut aries_problem, true);
    // println!("{}", format_partial_plan(&pb, &x)?);

    let result: LValue = if let Some(x) = &result {
        let plan = solver::extract_plan(x);
        println!("plan:\n{}\n{}", plan.format(), plan.format_hierarchy());
        //let first_task_id = plan.get_first_subtask().unwrap();
        /*let subplan = plan.extract_sub_plan(first_task_id);
        println!(
            "subplan: \n{}\n{}",
            subplan.format(),
            subplan.format_hierarchy()
        );*/
        solver::extract_instantiated_methods(x)?
    } else {
        string!("no solution found".to_string())
    };

    Ok(result)
}
