use crate::context::refinement::task_collection::TaskStatus::{Failure, Running};
use crate::context::refinement::task_collection::{
    AbstractTaskMetaData, RefinementMetaData, TaskMetaData, TaskMetaDataView, TaskStatus,
};
use crate::module::rae_exec::error::RaeExecError;
use crate::module::rae_exec::planning::{CtxPlanning, MOD_PLANNING};
use crate::module::rae_exec::{CtxRaeExec, MOD_RAE_EXEC, PARENT_TASK};
use crate::supervisor::options::{Planner, SelectMode};
use ::macro_rules_attribute::macro_rules_attribute;
use async_recursion::async_recursion;
use log::{error, info};
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::modules::utils::enr;
use ompas_utils::dyn_async;
use std::convert::{TryFrom, TryInto};

pub const REFINE: &str = "refine";
pub const RETRY: &str = "retry";
pub const RAE_SET_SUCCESS_FOR_TASK: &str = "rae-set-success-for-task";
pub const LAMBDA_RAE_EXEC_TASK: &str = "(define rae-exec-task 
    (lambda task
        (begin
            (define result (enr (cons 'refine task)))
            (if (err? result)
                result
                (let ((method (first result))
                      (task_id (second result)))

                    (begin
                        (define parent_task task_id)
                        (print \"Trying \" method \" for \" task_id)
                        (if (err? (enr method))
                            (rae-retry task_id)
                            (rae-set-success-for-task task_id))))))))";

pub const LAMBDA_RAE_RETRY: &str = "(define rae-retry 
    (lambda (task_id)
        (begin
            (define result (retry task_id))
            (if (err? result)
                result
                (begin
                    (if (err? (enr result))
                        (rae-retry task_id)
                        (rae-set-success-for-task task_id)))))))";

#[macro_rules_attribute(dyn_async !)]
pub async fn refine<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let task_label: LValue = args.into();
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let parent_task: Option<usize> = env
        .get_ref_symbol(PARENT_TASK)
        .map(|n| LNumber::try_from(n).unwrap().into());
    let mut task: AbstractTaskMetaData = ctx
        .agenda
        .add_abstract_task(task_label.clone(), parent_task)
        .await;
    let task_id = task.get_id();
    let result: LValue = select(&mut task, env).await?;

    let first_m = result;

    let result: LValue = if first_m == LValue::Nil {
        error!("No applicable method for task {}({})", task_label, task_id,);
        task.update_status(Failure);
        ctx.agenda.update_task(task.get_id(), task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(Running);
        ctx.agenda.update_task(task.get_id(), task).await;
        info!("Trying {} for task {}", first_m, task_id);
        let mut env = env.clone();
        //env.insert(PARENT_TASK, task_id.into());
        //println!("in env: {}", env.get_ref_symbol(PARENT_TASK).unwrap());
        vec![first_m, task_id.into()].into()
        /*let result: LResult = enr(&[first_m], &mut env).await;
        match result {
            Ok(lv) => {
                if matches!(lv, LValue::Err(_)) {
                    retry(task_id, &mut env).await?
                } else {
                    set_success_for_task(task_id, &env).await?
                }
            }
            Err(e) => {
                error!("error in evaluation of {}({}): {}", task_label, task_id, e);
                RaeExecError::EvaluationError.into()
            }
        }*/
    };

    Ok(result)
}

#[macro_rules_attribute(dyn_async !)]
pub async fn set_success_for_task<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */
    let task_id: i64 = (&args[0]).try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut task: TaskMetaData = ctx.agenda.trc.get(task_id).await;
    task.update_status(TaskStatus::Done).await;
    task.set_end_timepoint(ctx.agenda.get_instant());
    ctx.agenda.update_task(task.get_id(), task).await;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[macro_rules_attribute(dyn_async !)]
pub async fn retry<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let task_id: i64 = (&args[0]).try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut task: AbstractTaskMetaData = ctx.agenda.get_abstract_task(task_id as usize).await?;
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
        ctx.agenda.update_task(task.get_id(), task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.set_current_method(new_method.clone());
        ctx.agenda.update_task(task.get_id(), task).await;
        new_method
        /*let result: LResult = enr(&[new_method], env).await;

        match result {
            Ok(lv) => {
                if matches!(lv, LValue::Err(_)) {
                    retry(task_id, env).await?
                } else {
                    set_success_for_task(task_id, &env).await?
                }
            }
            Err(e) => {
                error!("error in evaluation of {}: {}", task_label, e);
                RaeExecError::EvaluationError.into()
            }
        }*/
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
    use crate::context::rae_env::{
        RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP,
        RAE_TASK_METHODS_MAP,
    };
    use crate::context::rae_state::RAEStateSnapshot;
    use crate::context::refinement::task_collection::{AbstractTaskMetaData, RefinementMetaData};
    use crate::context::refinement::Interval;
    use crate::module::rae_exec::planning::{CtxPlanning, MOD_PLANNING};
    use crate::module::rae_exec::platform::instance;
    use crate::module::rae_exec::{CtxRaeExec, MOD_RAE_EXEC, PARENT_TASK, RAE_SELECT, STATE};
    use crate::planning::binding_aries::solver::run_solver_for_htn;
    use crate::planning::binding_aries::{generate_chronicles, solver};
    use crate::planning::plan::AbstractTaskInstance;
    use crate::planning::structs::{ConversionContext, Problem};
    use crate::supervisor::options::Planner::Aries;
    use crate::supervisor::options::SelectMode;
    use log::info;
    use ompas_lisp::core::root_module::get;
    use ompas_lisp::core::root_module::list::cons;
    use ompas_lisp::core::structs::lenv::LEnv;
    use ompas_lisp::core::structs::lerror;
    use ompas_lisp::core::structs::lerror::LError::SpecialError;
    use ompas_lisp::core::structs::lnumber::LNumber;
    use ompas_lisp::core::structs::lvalue::LValue;
    use ompas_lisp::modules::utils::{enr, enumerate};
    use rand::seq::SliceRandom;
    use std::convert::{TryFrom, TryInto};
    use std::time::Instant;

    //pub const GREEDY_SELECT: &str = "greedy_select";

    //Returns the method to do.
    pub async fn planning_select(
        state: RAEStateSnapshot,
        tried: &Vec<LValue>,
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
                    ctx.agenda.get_abstract_task(parent_id).await?;
                let n = ctx.agenda.get_number_of_subtasks(parent_id).await - 1;
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

                    let refinement: AbstractTaskInstance =
                        match plan.chronicles.get(&task_id).unwrap().clone().try_into() {
                            Ok(a) => a,
                            Err(_) => {
                                return Err(SpecialError(
                                    RAE_SELECT,
                                    format!("task {} is not an abstract task", n),
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
            "Time to run solver: {} (optimize = {})",
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
        tried: &Vec<LValue>,
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
        let params: Vec<LValue> = task[1..]
            .iter()
            .map(|lv| LValue::List(vec![lv.clone()]))
            .collect();

        let mut applicable_methods: Vec<(LValue, i64)> = vec![];

        let mut env = env.clone();

        env.insert(STATE, state.into());
        let env = &env;

        let methods_template: Vec<LValue> = get(
            &[
                env.get_symbol(RAE_TASK_METHODS_MAP).unwrap(),
                task_label.clone(),
            ],
            env,
        )?
        .try_into()?;

        for template in methods_template {
            let types: Vec<LValue> = get(
                &[
                    env.get_symbol(RAE_METHOD_TYPES_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?
            .try_into()?;

            let score_lambda = get(
                &[
                    env.get_symbol(RAE_METHOD_SCORE_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?;

            let pre_conditions_lambda = get(
                &[
                    env.get_symbol(RAE_METHOD_PRE_CONDITIONS_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?;

            let mut instances_template = vec![template.clone()];
            instances_template.append(&mut params.clone());

            for t in &types[params.len()..] {
                instances_template.push(instance(&[t.clone()], env).await?);
            }

            let mut instances_template: Vec<LValue> =
                enumerate(&instances_template, env)?.try_into()?;

            /*println!(
                "instances for template {}: {}",
                template,
                LValue::from(instances_template.clone())
            );*/

            let iter = instances_template.drain(..);

            for i in iter {
                let i_vec: Vec<LValue> = (&i).try_into()?;
                let arg = cons(&[pre_conditions_lambda.clone(), i_vec[1..].into()], env)?;
                let lv: LValue = enr(&[arg], &mut env.clone()).await?;
                if !matches!(lv, LValue::Err(_)) {
                    let arg = cons(&[score_lambda.clone(), i_vec[1..].into()], env)?;
                    let score: i64 = enr(&[arg], &mut env.clone()).await?.try_into()?;
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
