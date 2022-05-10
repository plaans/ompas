use crate::rae_exec::{CtxRaeExec, MOD_RAE_EXEC, PARENT_TASK};
use log::{error, info};
use ompas_rae_core::error::RaeExecError;
use ompas_rae_core::planning::{CtxPlanning, MOD_PLANNING};
use ompas_rae_structs::options::{Planner, SelectMode};
use ompas_rae_structs::task_collection::TaskStatus::*;
use ompas_rae_structs::task_collection::{
    AbstractTaskMetaData, RefinementMetaData, TaskMetaData, TaskMetaDataView, TaskStatus,
};
use sompas_macros::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::convert::{TryFrom, TryInto};

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task_label: LValue = args.into();
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let parent_task: Option<usize> = env
        .get_ref_symbol(PARENT_TASK)
        .map(|n| LNumber::try_from(n).unwrap().into());
    let mut task: AbstractTaskMetaData = ctx
        .agenda
        .add_abstract_task(task_label.clone(), parent_task)
        .await;
    let task_id = *task.get_id();
    let result: LValue = select(&mut task, env).await?;

    let first_m = result;

    let result: LValue = if first_m == LValue::Nil {
        error!("No applicable method for task {}({})", task_label, task_id,);
        task.update_status(Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(Running);
        ctx.agenda.update_task(&task_id, task).await;
        vec![first_m, task_id.into()].into()
    };

    Ok(result)
}

#[async_scheme_fn]
pub async fn set_success_for_task(env: &LEnv, args: &[LValue]) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */
    let task_id: i64 = (&args[0]).try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut task: TaskMetaData = ctx.agenda.trc.get(&task_id).await;
    task.update_status(TaskStatus::Done).await;
    task.set_end_timepoint(ctx.agenda.get_instant());
    ctx.agenda.update_task(&task.get_id(), task).await;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[async_scheme_fn]
pub async fn retry(env: &LEnv, args: &[LValue]) -> LResult {
    let task_id: i64 = (&args[0]).try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut task: AbstractTaskMetaData = ctx.agenda.get_abstract_task(&(task_id as usize)).await?;
    let task_label = task.get_label().clone();
    error!("Retrying task {}({})", task_label, task_id);
    task.add_tried_method(task.get_current_method().clone());
    task.set_current_method(LValue::Nil);
    let new_method: LValue = select(&mut task, env).await?;
    let result: LValue = if new_method == LValue::Nil {
        error!(
            "No more method for task {}({}). Task is a failure!",
            task_label, task_id
        );
        task.update_status(Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.set_current_method(new_method.clone());
        ctx.agenda.update_task(&task_id, task).await;
        new_method
    };

    Ok(result)
}

/*const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";*/

pub async fn select(stack: &mut AbstractTaskMetaData, env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */
    let task_id = stack.get_id();
    let ctx_planning = env.get_context::<CtxPlanning>(MOD_PLANNING)?;
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let state = ctx.state.get_snapshot().await;

    let task: Vec<LValue> = stack.get_label().try_into()?;
    let tried = stack.get_tried();
    let rmd: RefinementMetaData = match &ctx_planning.select_mode {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            info!("select greedy for {}", stack.get_label());
            select::greedy_select(state, tried, task, env).await?
        }
        SelectMode::Planning(Planner::Aries, bool) => {
            info!("select with aries for {}", stack.get_label());
            select::planning_select(state, tried, task, env, *bool).await?
        }
        _ => todo!(),
    };

    info!(
        "sorted_methods for {}({}): {}",
        stack.get_label(),
        task_id,
        LValue::from(&rmd.applicable_methods)
    );

    let method = rmd.choosed.clone();

    stack.set_current_method(method.clone());
    stack.add_refinement(rmd);
    Ok(method)
}

mod select {
    use super::*;
    use crate::rae_exec::platform::instance;
    use crate::rae_exec::STATE;
    use ompas_rae_language::RAE_SELECT;
    use ompas_rae_planning::binding_aries::solver::run_solver_for_htn;
    use ompas_rae_planning::binding_aries::{generate_chronicles, solver};

    use ompas_rae_planning::structs::{ConversionContext, Problem};
    use ompas_rae_structs::interval::Interval;
    use ompas_rae_structs::options::Planner::Aries;
    use ompas_rae_structs::plan::AbstractTaskInstance;
    use ompas_rae_structs::rae_env::{
        RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP,
        RAE_TASK_METHODS_MAP,
    };
    use ompas_rae_structs::rae_state::RAEStateSnapshot;
    use rand::prelude::SliceRandom;
    use sompas_core::modules::get;
    use sompas_core::modules::list::cons;
    use sompas_modules::utils::{enr, enumerate};
    use sompas_structs::{lerror, list};
    use std::time::Instant;

    //pub const GREEDY_SELECT: &str = "greedy_select";

    //Returns the method to do.
    pub async fn planning_select(
        state: RAEStateSnapshot,
        tried: &[LValue],
        task: Vec<LValue>,
        env: &LEnv,
        optimize: bool,
    ) -> lerror::Result<RefinementMetaData> {
        let mut greedy: RefinementMetaData =
            greedy_select(state.clone(), tried, task.clone(), env).await?;

        println!("\n\nTask to plan: {}", LValue::from(task.clone()));
        println!("\t*tried: {}", LValue::from(tried));
        println!("\t*greedy: {}", LValue::from(&greedy.applicable_methods));
        let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

        match env
            .get_ref_symbol(PARENT_TASK)
            .map(|n| LNumber::try_from(n).unwrap().into())
        {
            Some(parent_id) => {
                let parent_stack: AbstractTaskMetaData =
                    ctx.agenda.get_abstract_task(&parent_id).await?;
                let n = ctx.agenda.get_number_of_subtasks(&parent_id).await - 1;
                println!("{} subtask of {}", n + 1, parent_id);
                println!("Searching for a generated plan...");
                if let Some(plan) = &parent_stack.get_last_refinement().plan {
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
                                return Err(lerror!(
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
                        greedy.refinement_type = SelectMode::Planning(Aries, optimize);
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

        let ctx_domain = env.get_context::<CtxPlanning>(MOD_PLANNING)?;

        let context = ConversionContext {
            domain: ctx_domain.domain.clone(),
            env: ctx_domain.env.clone(),
            state,
        };

        let mut problem: Problem = (&context).into();
        //let cc = convert_domain_to_chronicle_hierarchy(context)?;
        //println!("cc: {}", cc);
        problem.cc = ctx_domain.cc.as_ref().unwrap().clone();
        problem.goal_tasks.push(task.into());

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
            greedy.refinement_type = SelectMode::Planning(Aries, optimize);
            Ok(greedy)
        } else {
            Ok(greedy)
        }
    }

    pub async fn greedy_select(
        state: RAEStateSnapshot,
        tried: &[LValue],
        task: Vec<LValue>,
        env: &LEnv,
    ) -> lerror::Result<RefinementMetaData> {
        /*
        Steps:
        - Create a new entry in the agenda
        - Generate all instances of applicable methods
        - Select the best method
        - Store the stack
        - Return (best_method, task_id)
         */
        let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

        //println!("task to test in greedy: {}", LValue::from(task.clone()));
        let start = ctx.agenda.get_instant();

        let task_label = &task[0];
        //let task_string = LValue::from(task.clone()).to_string();
        let params: Vec<LValue> = task[1..].iter().map(|lv| list![lv.clone()]).collect();

        let mut applicable_methods: Vec<(LValue, i64)> = vec![];

        let mut env = env.clone();

        env.insert(STATE, state.into());
        let env = &env;

        let methods_template: Vec<LValue> = get(
            env,
            &[
                env.get_symbol(RAE_TASK_METHODS_MAP).unwrap(),
                task_label.clone(),
            ],
        )?
        .try_into()?;

        for template in methods_template {
            let types: Vec<LValue> = get(
                env,
                &[
                    env.get_symbol(RAE_METHOD_TYPES_MAP).unwrap(),
                    template.clone(),
                ],
            )?
            .try_into()?;

            let score_lambda = get(
                env,
                &[
                    env.get_symbol(RAE_METHOD_SCORE_MAP).unwrap(),
                    template.clone(),
                ],
            )?;

            let pre_conditions_lambda = get(
                env,
                &[
                    env.get_symbol(RAE_METHOD_PRE_CONDITIONS_MAP).unwrap(),
                    template.clone(),
                ],
            )?;

            let mut instances_template = vec![template.clone()];
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
                let i_vec: Vec<LValue> = (&i).try_into()?;
                let arg = cons(env, &[pre_conditions_lambda.clone(), i_vec[1..].into()])?;
                let lv: LValue = enr(env, &[arg]).await?;
                if !matches!(lv, LValue::Err(_)) {
                    let arg = cons(env, &[score_lambda.clone(), i_vec[1..].into()])?;
                    let score: i64 = enr(env, &[arg]).await?.try_into()?;
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
}

#[allow(non_snake_case, dead_code)]
pub fn RAEPlan(_: LValue) {}
