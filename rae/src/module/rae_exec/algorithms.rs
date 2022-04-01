use crate::context::actions_progress::Status;
use crate::context::actions_progress::Status::{Failure, Running};
use crate::context::agenda::TaskRefinement;
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

pub const RAE_REFINE: &str = "refine";

/*const LAMBDA_PROGRESS: &str = "
(define progress (lambda task
    (let* ((result (select task))
            (first_m (first result))
            (task_id (second result)))

            (if (null? first_m)
                (err err::no-applicable-method)
                (begin
                    (print \"trying \" first_m)
                    (define result (enr first_m))
                    (print \"tried fist method of \" task_id)
                    (if (err? result)
                        (retry task_id)
                        (rae-set-success-for-task task_id)))))))";*/

//Access part of the environment

/*const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get rae-task-methods-map label)))";*/
#[macro_rules_attribute(dyn_async !)]
pub async fn refine<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let task: LValue = args.into();
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let parent_task: Option<usize> = env
        .get_ref_symbol(PARENT_TASK)
        .map(|n| LNumber::try_from(n).unwrap().into());
    let mut stack: TaskRefinement = ctx.agenda.add_task(task.clone(), parent_task).await;
    let task_id = stack.get_task_id();
    let result: LValue = select(&mut stack, env).await?;

    let first_m = result;

    let result: LValue = if first_m == LValue::Nil {
        error!("No applicable method for task {}({})", task, task_id,);
        stack.set_status(Failure);
        ctx.agenda.update_refinement(stack).await?;
        RaeExecError::NoApplicableMethod.into()
    } else {
        stack.set_status(Running);
        ctx.agenda.update_refinement(stack).await?;
        info!("Trying {} for task {}", first_m, task_id);
        let mut env = env.clone();
        env.insert(PARENT_TASK, task_id.into());
        //println!("in env: {}", env.get_ref_symbol(PARENT_TASK).unwrap());
        let result: LResult = enr(&[first_m], &mut env).await;
        match result {
            Ok(lv) => {
                if matches!(lv, LValue::Err(_)) {
                    retry(task_id, &mut env).await?
                } else {
                    set_success_for_task(task_id, &env).await?
                }
            }
            Err(e) => {
                error!("error in evaluation of {}({}): {}", task, task_id, e);
                RaeExecError::EvaluationError.into()
            }
        }
    };

    Ok(result)
}

/*const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin
            (print \"Retrying task \" task_id)
            (if (null? new_method) ; if there is no method applicable
            nil
            (if (enr new_method)
                (rae-set-success-for-task task_id)
                (rae-retry task_id)))))))";*/

#[async_recursion]
pub async fn retry(task_id: usize, env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut stack: TaskRefinement = ctx.agenda.get_refinement(task_id as usize).await?;
    let task = stack.get_task().clone();
    error!("Retrying task {}({})", task, task_id);
    stack.add_tried_method(stack.get_current_method().clone());
    stack.set_current_method(LValue::Nil);
    let new_method: LValue = select(&mut stack, env).await?;
    let result: LValue = if new_method == LValue::Nil {
        error!(
            "No more method for task {}({}). Task is a failure!",
            task, task_id
        );
        stack.set_status(Failure);
        ctx.agenda.update_refinement(stack).await?;
        RaeExecError::NoApplicableMethod.into()
    } else {
        stack.set_current_method(new_method.clone());
        ctx.agenda.update_refinement(stack).await?;
        let result: LResult = enr(&[new_method], env).await;

        match result {
            Ok(lv) => {
                if matches!(lv, LValue::Err(_)) {
                    retry(task_id, env).await?
                } else {
                    set_success_for_task(task_id, &env).await?
                }
            }
            Err(e) => {
                error!("error in evaluation of {}: {}", task, e);
                RaeExecError::EvaluationError.into()
            }
        }
    };

    Ok(result)
}

/*const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";*/

pub async fn select(stack: &mut TaskRefinement, env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */
    let task_id = stack.get_task_id();
    let ctx_planning = env.get_context::<CtxPlanning>(MOD_PLANNING)?;

    let task: Vec<LValue> = stack.get_task().try_into()?;

    let methods: LValue = match &ctx_planning.select_mode {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            info!("select greedy for {}", stack.get_task());
            select::greedy_select(task, env).await?
        }
        SelectMode::Planning(Planner::Aries) => {
            info!("select with aries for {}", stack.get_task());
            select::planning_select(task, env).await?
        }
        _ => todo!(),
    };

    if let LValue::List(mut methods) = methods {
        let tried = stack.get_tried();
        methods.retain(|lv| !tried.contains(lv));
        info!(
            "sorted_methods for {}({}): {}",
            stack.get_task(),
            task_id,
            LValue::from(&methods)
        );
        if !methods.is_empty() {
            stack.set_current_method(methods[0].clone());
            stack.set_applicable_methods(methods[1..].into());
            Ok(methods[0].clone())
        } else {
            Ok(LValue::Nil)
        }
    } else {
        Ok(LValue::Nil)
    }
}

mod select {
    use crate::context::rae_env::{
        RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP,
        RAE_TASK_METHODS_MAP,
    };
    use crate::module::rae_exec::planning::{CtxPlanning, MOD_PLANNING};
    use crate::module::rae_exec::platform::instance;
    use crate::module::rae_exec::{get_facts, CtxRaeExec, MOD_RAE_EXEC, STATE};
    use crate::planning::binding_aries::solver::run_solver;
    use crate::planning::binding_aries::{build_chronicles, solver};
    use crate::planning::conversion::convert_domain_to_chronicle_hierarchy;
    use crate::planning::structs::{ConversionContext, Problem};
    use ompas_lisp::core::eval;
    use ompas_lisp::core::root_module::get;
    use ompas_lisp::core::root_module::list::cons;
    use ompas_lisp::core::structs::lenv::LEnv;
    use ompas_lisp::core::structs::lerror::LResult;
    use ompas_lisp::core::structs::lvalue::LValue;
    use ompas_lisp::modules::utils::{enr, enumerate};
    use std::convert::TryInto;

    //pub const GREEDY_SELECT: &str = "greedy_select";

    //Returns the method to do.
    pub async fn planning_select(task: Vec<LValue>, env: &LEnv) -> LResult {
        let greedy_methods = greedy_select(task.clone(), env);

        println!("task to plan: {}", LValue::from(task.clone()));
        let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

        let ctx_domain = env.get_context::<CtxPlanning>(MOD_PLANNING)?;

        let context = ConversionContext {
            domain: ctx_domain.domain.clone(),
            env: ctx_domain.env.clone(),
            state: ctx.state.get_snapshot().await,
        };

        let mut problem: Problem = (&context).into();
        let cc = convert_domain_to_chronicle_hierarchy(context)?;
        println!("cc: {}", cc);
        problem.cc = cc;
        problem.goal_tasks.push(task.into());

        let mut aries_problem = build_chronicles(&problem)?;

        let result = run_solver(&mut aries_problem, true);
        // println!("{}", format_partial_plan(&pb, &x)?);

        let result: LValue = if let Some(x) = &result {
            let greedy = greedy_methods.await?;
            println!("greedy methods: {}", greedy);
            let mut greedy_methods: Vec<LValue> = greedy.try_into()?;
            let planner_methods = solver::extract_instantiated_methods(x)?;
            let result: Vec<LValue> = planner_methods.try_into()?;
            let planner_method = result[0].clone();
            println!("planners methods: {}", planner_method);

            greedy_methods.retain(|m| m != &planner_method);
            /*for i in 0..greedy_methods.len() {
                if greedy_methods[i] == planner_method {
                    greedy_methods.remove(i);
                    break;
                }
            }*/
            let mut result = vec![planner_method];
            result.append(&mut greedy_methods);
            result.into()
        } else {
            LValue::String("no solution found".to_string())
        };

        Ok(result)
    }

    pub async fn greedy_select(task: Vec<LValue>, env: &LEnv) -> LResult {
        /*
        Steps:
        - Create a new entry in the agenda
        - Generate all instances of applicable methods
        - Select the best method
        - Store the stack
        - Return (best_method, task_id)
         */

        //println!("task to test in greedy: {}", LValue::from(task.clone()));

        let task_label = &task[0];
        let task_string = LValue::from(task.clone()).to_string();
        let params: Vec<LValue> = task[1..]
            .iter()
            .map(|lv| LValue::List(vec![lv.clone()]))
            .collect();

        let mut applicable_methods: Vec<(LValue, i64)> = vec![];
        let state: LValue = get_facts(&[], env).await?;

        let mut env = env.clone();

        env.insert(STATE, state);
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

        applicable_methods.sort_by(|a, b| a.1.cmp(&b.1));
        let methods: Vec<_> = applicable_methods.drain(..).map(|a| a.0).collect();

        Ok(methods.into())
    }
}

async fn set_success_for_task(task_id: usize, env: &LEnv) -> Result<LValue, LError> {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mut refinement: TaskRefinement = ctx.agenda.get_refinement(task_id).await?;
    refinement.set_status(Status::Done);
    ctx.agenda.update_refinement(refinement).await?;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[allow(non_snake_case, dead_code)]
pub fn RAEPlan(_: LValue) {}
