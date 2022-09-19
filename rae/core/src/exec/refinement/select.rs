use crate::contexts::ctx_domain::{CtxDomain, CTX_DOMAIN};
use crate::contexts::ctx_planning::{CtxPlanning, CTX_PLANNING};
use crate::contexts::ctx_rae::{CtxRae, CTX_RAE};
use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::contexts::ctx_task::{CtxTask, CTX_TASK};
use crate::exec::refinement::c_choice::{create_env, CtxCChoice};
use crate::exec::{instance, STATE};
use log::info;
use ompas_rae_language::*;
use ompas_rae_planning::aries::binding::solver::run_solver_for_htn;
use ompas_rae_planning::aries::binding::{generate_chronicles, solver};
use ompas_rae_planning::aries::structs::{ConversionContext, Problem};
use ompas_rae_structs::interval::Interval;
use ompas_rae_structs::plan::AbstractTaskInstance;
use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::task_state::{AbstractTaskMetaData, RefinementMetaData};
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use rand::prelude::SliceRandom;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_modules::utils::enumerate;
use sompas_structs::contextcollection::Context;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror};
use std::borrow::Borrow;
use std::time::Instant;

//Returns the method to do.
pub async fn aries_select(
    state: WorldStateSnapshot,
    tried: &[LValue],
    task: Vec<LValue>,
    env: &LEnv,
    optimize: bool,
) -> lruntimeerror::Result<RefinementMetaData> {
    let mut greedy: RefinementMetaData =
        greedy_select(state.clone(), tried, task.clone(), env).await?;

    println!("\n\nTask to plan: {}", LValue::from(task.clone()));
    println!("\t*tried: {}", LValue::from(tried));
    println!("\t*greedy: {}", LValue::from(&greedy.applicable_methods));
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let parent_task = env.get_context::<CtxTask>(CTX_TASK)?.parent_id;
    match parent_task {
        Some(parent_id) => {
            let parent_stack: AbstractTaskMetaData =
                ctx.agenda.get_abstract_task(&parent_id).await?;
            let n = ctx.agenda.get_number_of_subtasks(&parent_id).await - 1;
            println!("{} subtask of {}", n + 1, parent_id);
            println!("Searching for a generated plan...");
            if let Some(plan) = &parent_stack.get_last_refinement().unwrap().plan {
                //Get number of subtasks for
                println!("Parent task has a plan!!!\n {}", plan.format_hierarchy());
                let root_id = plan.get_root_task().unwrap();
                let instance: AbstractTaskInstance = plan
                    .chronicles
                    .get(&root_id)
                    .unwrap()
                    .clone()
                    .try_into()
                    .expect("root task is not an abstract task");
                let task_id = instance.subtasks[n];
                println!("subtask {:?}: ", plan.chronicles.get(&task_id).unwrap());
                let refinement: AbstractTaskInstance =
                    match plan.chronicles.get(&task_id).unwrap().clone().try_into() {
                        Ok(a) => a,
                        Err(_) => {
                            return Err(lruntimeerror!(
                                RAE_SELECT,
                                format!("task {} is not an abstract task:", n)
                            ))
                        }
                    };

                let task_to_refine = LValue::from(&task);
                println!(
                    "\n* Previous plan propose: ({}) -> {}",
                    refinement.task, refinement.method,
                );
                println!("We verify that the method is still applicable...");

                let planner_method = refinement.method;

                if refinement.task == task_to_refine
                    && !tried.contains(&refinement.task)
                    && greedy.applicable_methods.contains(&planner_method)
                {
                    let subtask_plan = plan.extract_sub_plan(task_id);
                    println!("Method is applicable! We can bypass the planner.");

                    println!("*Plan for subtask:\n{}", subtask_plan.format_hierarchy());
                    greedy.applicable_methods.retain(|m| m != &planner_method);

                    let mut applicable_methods = vec![planner_method];
                    applicable_methods.append(&mut greedy.applicable_methods);
                    greedy.plan = Some(subtask_plan);
                    greedy.choosed = applicable_methods.get(0).cloned().unwrap_or(LValue::Nil);
                    greedy.applicable_methods = applicable_methods;
                    greedy.interval.set_end(ctx.agenda.get_instant());
                    greedy.refinement_type = SelectMode::Planning(Planner::Aries, optimize);
                    return Ok(greedy);
                } else {
                    println!("Error in continuum, we are going to plan...");
                    println!("State: {}", LValue::from(state.clone()))
                }
            } else {
                println!("No plan available for parent task...A plan is needed!")
            }
        }
        None => println!("Root task, a plan is needed."),
    };

    let ctx_domain = env.get_context::<CtxPlanning>(CTX_PLANNING)?;

    let context = ConversionContext {
        domain: ctx_domain.domain.clone(),
        env: ctx_domain.env.clone(),
        state,
    };

    let mut problem: Problem = context.borrow().into();
    //let cc = convert_domain_to_chronicle_hierarchy(context)?;
    //println!("cc: {}", cc);
    problem.cc = ctx_domain.cc.as_ref().unwrap().clone();
    problem.goal_tasks.push(LValue::from(task).try_into()?);

    let mut aries_problem = generate_chronicles(&problem)?;
    let instant = Instant::now();
    let result = run_solver_for_htn(&mut aries_problem, optimize);
    info!(
        "Time to run solver: {:^3} ms (optimize = {})",
        instant.elapsed().as_micros() as f64 / 1000.0,
        optimize
    );
    // println!("{}", format_partial_plan(&pb, &x)?);

    let mut greedy: RefinementMetaData = greedy;

    if let Some(x) = &result {
        let plan = solver::extract_plan(x);
        let first_task_id = plan.get_first_subtask().unwrap();
        let method_plan = plan.extract_sub_plan(first_task_id);
        let task: AbstractTaskInstance = plan
            .chronicles
            .get(&first_task_id)
            .unwrap()
            .clone()
            .try_into()?;

        greedy.plan = Some(method_plan);

        let mut greedy_methods = greedy.applicable_methods.clone();

        //let planner_methods = solver::extract_instantiated_methods(x)?;
        //let result: Vec<LValue> = planner_methods.try_into()?;
        //let planner_method = result[0].clone();

        let planner_method = task.method;
        println!("planner method: {}", planner_method);
        if greedy_methods.contains(&planner_method) {
            greedy_methods.retain(|m| m != &planner_method);
        } else {
            panic!("planner found a non applicable method...")
        }

        let mut applicable_methods = vec![planner_method];
        applicable_methods.append(&mut greedy_methods);
        applicable_methods.retain(|m| !tried.contains(m));

        greedy.choosed = applicable_methods.get(0).cloned().unwrap_or(LValue::Nil);
        greedy.applicable_methods = applicable_methods;
        greedy.interval.set_end(ctx.agenda.get_instant());
        greedy.refinement_type = SelectMode::Planning(Planner::Aries, optimize);
        Ok(greedy)
    } else {
        Ok(greedy)
    }
}

pub async fn greedy_select(
    state: WorldStateSnapshot,
    tried: &[LValue],
    task: Vec<LValue>,
    env: &LEnv,
) -> lruntimeerror::Result<RefinementMetaData> {
    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (best_method, task_id)
     */
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let ctx_domain = env.get_context::<CtxDomain>(CTX_DOMAIN)?;

    //println!("task to test in greedy: {}", LValue::from(task.clone()));
    let start = ctx.agenda.get_instant();

    let task_label = task[0].to_string();
    //let task_string = LValue::from(task.clone()).to_string();
    let params: Vec<LValue> = task[1..].iter().map(|lv| list![lv.clone()]).collect();

    let mut applicable_methods: Vec<(LValue, i64)> = vec![];

    let mut env = env.clone();

    env.insert(STATE, state.into());
    let env = &env;

    let method_templates: Vec<String> = ctx_domain
        .domain
        .tasks
        .get(&task_label)
        .unwrap()
        .get_methods()
        .clone();

    for template in &method_templates {
        let method_template = ctx_domain.domain.methods.get(template).unwrap();
        let types: Vec<LValue> = method_template
            .parameters
            .get_types_as_lvalue()
            .try_into()?;
        /*let types: Vec<LValue> = get(
            env,
            &[
                env.get_symbol(RAE_METHOD_TYPES_MAP).unwrap(),
                template.clone(),
            ],
        )?
        .try_into()?;*/

        let score_lambda = method_template.lambda_score.clone();
        let pre_conditions_lambda = method_template.lambda_pre_conditions.clone();

        let mut instances_template = vec![template.into()];
        instances_template.append(&mut params.clone());

        for t in &types[params.len()..] {
            instances_template.push(instance(env, &[t.clone()]).await?);
        }

        let mut instances_template: Vec<LValue> =
            enumerate(env, &instances_template)?.try_into()?;

        /*println!(
            "instances for template {}: {}",
            template,
            LValue::from(instances_template.clone())
        );*/

        let iter = instances_template.drain(..);

        for i in iter {
            let i_vec: Vec<LValue> = i.borrow().try_into()?;
            let arg = cons(env, &[pre_conditions_lambda.clone(), i_vec[1..].into()])?;
            let arg_debug = arg.to_string();
            let lv: LValue = eval(
                &list!(
                    LCoreOperator::Enr.into(),
                    list!(LCoreOperator::Quote.into(), arg)
                ),
                &mut env.clone(),
                None,
            )
            .await
            .map_err(|e: LRuntimeError| e.chain(format!("eval pre_conditions: {}", arg_debug)))?;
            if !matches!(lv, LValue::Err(_)) {
                let arg = cons(env, &[score_lambda.clone(), i_vec[1..].into()])?;
                let arg_debug = arg.to_string();
                let score: i64 = eval(
                    &list!(
                        LCoreOperator::Enr.into(),
                        list!(LCoreOperator::Quote.into(), arg)
                    ),
                    &mut env.clone(),
                    None,
                )
                .await
                .map_err(|e: LRuntimeError| e.chain(format!("eval score: {}", arg_debug)))?
                .try_into()?;
                applicable_methods.push((i, score))
            }
        }
    }

    /*
    Sort the list
     */
    let mut rng = rand::thread_rng();
    applicable_methods.shuffle(&mut rng);
    applicable_methods.sort_by_key(|a| a.1);
    applicable_methods.reverse();
    let mut methods: Vec<_> = applicable_methods.drain(..).map(|a| a.0).collect();
    methods.retain(|m| !tried.contains(m));
    let choosed = methods.get(0).cloned().unwrap_or(LValue::Nil);

    Ok(RefinementMetaData {
        refinement_type: SelectMode::Greedy,
        applicable_methods: methods,
        choosed,
        plan: None,
        interval: Interval::new(start, Some(ctx.agenda.get_instant())),
    })
}

pub async fn c_choice_select(
    state: WorldStateSnapshot,
    tried: &[LValue],
    task: Vec<LValue>,
    env: &LEnv,
) -> lruntimeerror::Result<RefinementMetaData> {
    let new_env = env.clone();
    let domain = env.get_context::<CtxDomain>(CTX_DOMAIN).unwrap();

    let mut new_env: LEnv = create_env(new_env, &domain.domain).await;
    new_env.import_module(CtxCChoice::new(tried.to_vec()), WithoutPrefix);
    new_env.import_context(Context::new(CtxState::new(state.clone().into())), CTX_STATE);

    let mut greedy: RefinementMetaData = greedy_select(state, tried, task.clone(), env).await?;
    greedy.refinement_type = SelectMode::Planning(Planner::CChoice, false);

    let method: LValue = eval(&task.into(), &mut new_env, None).await?;

    greedy.choosed = method;
    greedy
        .interval
        .set_end(env.get_context::<CtxRae>(CTX_RAE)?.agenda.get_instant());

    Ok(greedy)
}
