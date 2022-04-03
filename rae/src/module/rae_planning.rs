use crate::module::{CtxRae, MOD_RAE};
use crate::planning::binding_aries::solver::run_solver;
use crate::planning::binding_aries::{build_chronicles, solver};
use crate::planning::conversion::convert_domain_to_chronicle_hierarchy;
use crate::planning::plan::TaskInstance;
use crate::planning::structs::{ConversionContext, Problem};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::dyn_async;

#[macro_rules_attribute(dyn_async!)]
pub async fn plan_task<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let task: LValue = args.into();
    println!("task to plan: {}", task);
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let mut problem: Problem = (&context).into();
    let cc = convert_domain_to_chronicle_hierarchy(context)?;
    println!("cc: {}", cc);
    problem.cc = cc;
    problem.goal_tasks.push(task.into());

    let mut aries_problem = build_chronicles(&problem)?;

    let result = run_solver(&mut aries_problem, true);
    // println!("{}", format_partial_plan(&pb, &x)?);

    let result: LValue = if let Some(x) = &result {
        let plan = solver::extract_plan(x);
        println!("{}\n{}", plan.format(), plan.format_hierarchy());
        let root = plan.chronicles.first().unwrap();
        if let TaskInstance::AbstractTaskInstance(a) = root {}
        solver::extract_instantiated_methods(x)?
    } else {
        LValue::String("no solution found".to_string())
    };

    Ok(result)
}
