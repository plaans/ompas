pub mod c_choice;
pub mod rae_plan;
pub mod select;

use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::action_state::{
    ActionMetaData, ActionMetaDataView, RefinementMetaData, TaskMetaData,
};
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;

use crate::contexts::ctx_planning::{CtxPlanning, CTX_PLANNING};
use crate::contexts::ctx_rae::{CtxOMPAS, CTX_RAE};
use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::contexts::ctx_task::{ModTask, CTX_TASK};
use crate::RaeExecError;
use ompas_rae_structs::state::action_status::ActionStatus;
use sompas_macros::async_scheme_fn;
use std::convert::{TryFrom, TryInto};

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task_label: LValue = args.into();
    let ctx = env.get_context::<CtxOMPAS>(CTX_RAE)?;
    let log = ctx.get_log_client();
    let parent_task: Option<usize> = match env.get_context::<ModTask>(CTX_TASK) {
        Ok(ctx) => ctx.parent_id,
        Err(_) => None,
    };
    let mut task: TaskMetaData = ctx.agenda.add_task(task_label.clone(), parent_task).await;
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
        task.update_status(ActionStatus::Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(ActionStatus::Running(None));
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

    let ctx = env.get_context::<CtxOMPAS>(CTX_RAE)?;
    let mut task: ActionMetaData = ctx.agenda.trc.get(&task_id).await;
    task.update_status(ActionStatus::Success).await;
    task.set_end_timepoint(ctx.agenda.get_instant());
    ctx.agenda.update_task(&task.get_id(), task).await;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[async_scheme_fn]
pub async fn retry(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<CtxOMPAS>(CTX_RAE)?;
    let log = ctx.get_log_client();
    let mut task: TaskMetaData = ctx.agenda.get_task(&(task_id as usize)).await?;
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
        task.update_status(ActionStatus::Failure);
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

pub async fn select(stack: &mut TaskMetaData, env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */
    let task_id = stack.get_id();
    let ctx_planning = env.get_context::<CtxPlanning>(CTX_PLANNING)?;
    let log = env.get_context::<CtxOMPAS>(CTX_RAE)?.get_log_client();
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
    (lambda __task__
        (begin
            (define __result__ (enr (cons 'refine __task__)))
            (if (err? __result__)
                __result__
                (let ((__method__ (first __result__))
                      (__task_id__ (second __result__)))

                    (begin
                        (define-parent-task __task_id__)
                        (print \"Trying \" __method__ \" for \" __task_id__)
                        (if (err? (enr __method__))
                            (rae-retry __task_id__)
                            (rae-set-success-for-task __task_id__))))))))";

pub const LAMBDA_RAE_RETRY: &str = "(define rae-retry
    (lambda (__task_id__)
        (begin
            (define __result__ (retry __task_id__))
            (if (err? __result__)
                __result__
                (begin
                    (if (err? (enr __result__))
                        (rae-retry __task_id__)
                        (rae-set-success-for-task __task_id__)))))))";

pub const LAMBDA_GET_PRECONDITIONS: &str = "(define get-preconditions\
    (lambda (__label__)\
        (get rae-method-pre-conditions-map __label__)))";

pub const LAMBDA_GET_SCORE: &str = "(define get-score\
    (lambda (__label__)\
        (get rae-method-score-map __label__)))";

pub const LAMBDA_GET_ACTION_MODEL: &str = "(define get-action-model
    (lambda (__label__)
        (get rae-action-model-map __label__)))";

pub const LAMBDA_EVAL_PRE_CONDITIONS: &str = "(define eval-pre-conditions
    (lambda (__method__)
        (sim_block
            (eval (cons (get-preconditions (car __method__)) (quote-list (cdr __method__)))))))";

pub const LAMBDA_COMPUTE_SCORE: &str = "(define compute-score
    (lambda (__method__)
        (sim_block
            (eval (cons (get-score (car __method__)) (quote-list (cdr __method__)))))))";

pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable?
    (lambda (method)
        (sim_block
            (eval-pre-conditions method))))";

pub const LAMBDA_GENERATE_APPLICABLE_INSTANCES: &str = "(define generate_applicable_instances
    (lambda (__task__)
        (let* ((__task_label__ (first __task__))
               (__params__ (cdr __task__))
               (__methods__ (get rae-task-methods-map __task_label__)))
            (r_generate_instances
                (enr (cons enumerate (cons __methods__ __params__)))))))";

pub const LAMBDA_R_GENERATE_INSTANCES: &str = "(define r_generate_instances
    (lambda (__methods__)
        (if (null? __methods__)
            nil
            (let* ((__method__ (car __methods__))
                    (__methods__ (cdr __methods__))
                    (__method_label__ (first __method__))
                    (__params__ (cdr __method__)))
                (begin
                    (define __types__ (get rae-method-types-map __method_label__))
                    (if (> (len __types__) (len __params__))
                        (begin
                            (define __instance_types__ (mapf instance (sublist __types__ (len __params__))))
                            (define __instances__ (enr (cons enumerate (append __method__ __instance_types__))))
                            (append (r_test_method __instances__) (r_generate_instances __methods__)))
                        (cons
                            (if (! (err? (eval-pre-conditions __method__)))
                                (list __method__ (compute-score __method__))
                                nil)
                            (r_generate_instances __methods__))))))))";
pub const LAMBDA_R_TEST_METHOD: &str = "(define r_test_method
    (lambda (__instances__)
        (if (null? __instances__)
            nil
            (if (eval-pre-conditions (car __instances__))
                (cons
                    (list (car __instances__) (compute-score (car __instances__)))
                    (r_test_method (cdr __instances__)))
                (r_test_method (cdr __instances__))))))";
