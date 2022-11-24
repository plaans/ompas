pub mod c_choice;
pub mod rae_plan;
pub mod select;

use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::task_state::{
    AbstractTaskMetaData, RefinementMetaData, TaskMetaData, TaskMetaDataView,
};
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;

use crate::contexts::ctx_planning::{CtxPlanning, CTX_PLANNING};
use crate::contexts::ctx_rae::{CtxRae, CTX_RAE};
use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::contexts::ctx_task::{CtxTask, CTX_TASK};
use crate::RaeExecError;
use ompas_rae_structs::state::task_status::TaskStatus;
use sompas_macros::async_scheme_fn;
use std::convert::{TryFrom, TryInto};

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task_label: LValue = args.into();
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let log = ctx.get_log_client();
    let parent_task: Option<usize> = match env.get_context::<CtxTask>(CTX_TASK) {
        Ok(ctx) => ctx.parent_id,
        Err(_) => None,
    };
    let mut task: AbstractTaskMetaData = ctx
        .agenda
        .add_abstract_task(task_label.clone(), parent_task)
        .await;
    let task_id = *task.get_id();
    let result: LValue = select(&mut task, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    let first_m = result;

    let result: LValue = if first_m == LValue::Nil {
        log.error(format!(
            "No applicable method for task {task_label}({task_id})"
        ))
        .await;
        task.update_status(TaskStatus::Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(TaskStatus::Running);
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
    let task_id: i64 = args[0].borrow().try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let mut task: TaskMetaData = ctx.agenda.trc.get(&task_id).await;
    task.update_status(TaskStatus::Done).await;
    task.set_end_timepoint(ctx.agenda.get_instant());
    ctx.agenda.update_task(&task.get_id(), task).await;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[async_scheme_fn]
pub async fn retry(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let log = ctx.get_log_client();
    let mut task: AbstractTaskMetaData = ctx.agenda.get_abstract_task(&(task_id as usize)).await?;
    let task_label = task.get_label().clone();
    log.error(format!("Retrying task {task_label}({task_id})"))
        .await;
    task.add_tried_method(task.get_current_method().clone());
    task.set_current_method(LValue::Nil);
    let new_method: LValue = select(&mut task, env).await?;
    let result: LValue = if new_method == LValue::Nil {
        log.error(format!(
            "No more method for task {task_label}({task_id}). Task is a failure!",
        ))
        .await;
        task.update_status(TaskStatus::Failure);
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
    let ctx_planning = env.get_context::<CtxPlanning>(CTX_PLANNING)?;
    let log = env.get_context::<CtxRae>(CTX_RAE)?.get_log_client();
    let state: WorldStateSnapshot = env
        .get_context::<CtxState>(CTX_STATE)?
        .state
        .get_snapshot()
        .await;

    let task: Vec<LValue> = stack.get_label().try_into()?;
    let tried = stack.get_tried();
    let rmd: RefinementMetaData = match &ctx_planning.select_mode {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            log.debug(format!("select greedy for {}", stack.get_label()))
                .await;
            select::greedy_select(state, tried, task, env)
                .await
                .map_err(|e| e.chain("greedy_select"))?
        }
        SelectMode::Planning(Planner::Aries(bool)) => {
            log.debug(format!("select with aries for {}", stack.get_label()))
                .await;
            select::aries_select(state, tried, task, env, *bool)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::CChoice(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            select::c_choice_select(state, tried, task, env, *config)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::RAEPlan(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            select::rae_plan_select(state, tried, task, env, *config)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        _ => todo!(),
    };

    log.debug(format!(
        "sorted_methods for {}({}): {}",
        stack.get_label(),
        task_id,
        LValue::from(&rmd.applicable_methods)
    ))
    .await;

    let method = rmd.choosed.clone();

    stack.set_current_method(method.clone());
    stack.add_refinement(rmd);
    Ok(method)
}

pub const LAMBDA_RAE_EXEC_TASK: &str = "(define exec-task
    (lambda task
        (begin
            (define result (enr (cons 'refine task)))
            (if (err? result)
                result
                (let ((method (first result))
                      (task_id (second result)))

                    (begin
                        (define-parent-task task_id)
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

pub const LAMBDA_GET_PRECONDITIONS: &str = "(define get-preconditions\
    (lambda (label)\
        (get rae-method-pre-conditions-map label)))";

pub const LAMBDA_GET_SCORE: &str = "(define get-score\
    (lambda (label)\
        (get rae-method-score-map label)))";

pub const LAMBDA_GET_ACTION_MODEL: &str = "(define get-action-model
    (lambda (label)
        (get rae-action-model-map label)))";

pub const LAMBDA_EVAL_PRE_CONDITIONS: &str = "(define eval-pre-conditions
    (lambda (method)
        (sim_block
            (eval (cons (get-preconditions (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_COMPUTE_SCORE: &str = "(define compute-score
    (lambda (method)
        (sim_block
            (eval (cons (get-score (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable?
    (lambda (method)
        (sim_block
            (eval-pre-conditions method))))";

pub const LAMBDA_GENERATE_APPLICABLE_INSTANCES: &str = "(define generate_applicable_instances
    (lambda (task)
        (let* ((task_label (first task))
               (params (cdr task))
               (methods (get rae-task-methods-map task_label)))
            (r_generate_instances
                (enr (cons enumerate (cons methods params)))))))";

pub const LAMBDA_R_GENERATE_INSTANCES: &str = "(define r_generate_instances
    (lambda (methods)
        (if (null? methods)
            nil
            (let* ((method (car methods))
                    (methods (cdr methods))
                    (method_label (first method))
                    (params (cdr method)))
                (begin
                    (define types (get rae-method-types-map method_label))
                    (if (> (len types) (len params))
                        (begin
                            (define instance_types (mapf instance (sublist types (len params))))
                            (define instances (enr (cons enumerate (append method instance_types))))
                            (append (r_test_method instances) (r_generate_instances methods)))
                        (cons
                            (if (! (err? (eval-pre-conditions method)))
                                (list method (compute-score method))
                                nil)
                            (r_generate_instances methods))))))))";
pub const LAMBDA_R_TEST_METHOD: &str = "(define r_test_method
    (lambda (instances)
        (if (null? instances)
            nil
            (if (eval-pre-conditions (car instances))
                (cons
                    (list (car instances) (compute-score (car instances)))
                    (r_test_method (cdr instances)))
                (r_test_method (cdr instances))))))";
