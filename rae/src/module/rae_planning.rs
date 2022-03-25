use crate::module::{CtxRae, MOD_RAE};
use crate::planning::binding_aries::build_chronicles;
use crate::planning::binding_aries::solver::{run_solver, PlanResult};
use crate::planning::conversion::convert_domain_to_chronicle_hierarchy;
use crate::planning::structs::{ConversionContext, Problem};
use ::macro_rules_attribute::macro_rules_attribute;
use aries_model::extensions::AssignmentExt;
use aries_model::lang::SAtom;
use aries_planning::chronicles::ChronicleKind;
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
        get_instantiated_methods(x)?
    } else {
        LValue::String("no solution found".to_string())
    };

    Ok(result)
}

fn get_instantiated_methods(pr: &PlanResult) -> LResult {
    let ass = &pr.ass;
    let problem = &pr.fp;

    let methods: Vec<_> = pr
        .fp
        .chronicles
        .iter()
        .filter(|ch| {
            ass.boolean_value_of(ch.chronicle.presence) == Some(true)
                && ch.chronicle.kind == ChronicleKind::Method
        })
        .collect();

    let fmt1 = |x: &SAtom| -> LValue {
        let sym = ass.sym_domain_of(*x).into_singleton().unwrap();
        problem.model.shape.symbols.symbol(sym).to_string().into()
    };
    let mut lv_methods: Vec<LValue> = vec![];
    for m in methods {
        let name: Vec<LValue> = m.chronicle.name.iter().map(|s| fmt1(s)).collect::<_>();
        lv_methods.push(name.into());
    }

    Ok(lv_methods.into())
}
