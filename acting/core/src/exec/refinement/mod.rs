pub mod aries;
pub mod c_choice;
pub mod rae_plan;

use crate::exec::acting_context::ModActingContext;
use crate::exec::refinement::aries::aries_select;
use crate::exec::refinement::c_choice::c_choice_select;
use crate::exec::refinement::rae_plan::rae_plan_select;
use crate::exec::state::{instance, ModState};
use crate::exec::ModExec;
use crate::RaeExecError;
use ompas_language::exec::acting_context::DEF_PROCESS_ID;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::refinement::*;
use ompas_language::exec::MOD_EXEC;
use ompas_middleware::logger::LogClient;
use ompas_planning::conversion::flow::annotate::annotate;
use ompas_planning::conversion::flow::p_eval::p_eval;
use ompas_planning::conversion::flow::p_eval::r#struct::PLEnv;
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::interface::rae_options::OMPASOptions;
use ompas_structs::interface::select_mode::{Planner, SelectMode};
use ompas_structs::state::world_state::{WorldState, WorldStateSnapshot};
use ompas_structs::supervisor::action_status::ActionStatus;
use ompas_structs::supervisor::inner::ProcessKind;
use ompas_structs::supervisor::interval::Interval;
use ompas_structs::supervisor::process::method::MethodProcess;
use ompas_structs::supervisor::process::process_ref::{Label, MethodLabel, ProcessRef};
use ompas_structs::supervisor::process::task::{
    RTSelect, Refinement, RefinementInner, SelectTrace, TaskProcess,
};
use ompas_structs::supervisor::{ActingProcessId, Supervisor};
use rand::prelude::SliceRandom;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_language::time::MOD_TIME;
use sompas_macros::async_scheme_fn;
use sompas_modules::time::ModTime;
use sompas_modules::utils::enumerate;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::{list, lruntimeerror};
use std::borrow::Borrow;
use std::convert::TryInto;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct ModRefinement {
    //pub env: LEnv,
    pub domain: Arc<RwLock<OMPASDomain>>,
    pub supervisor: Supervisor,
    pub state: WorldState,
    pub options: Arc<RwLock<OMPASOptions>>,
    pub log: LogClient,
}

impl ModRefinement {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            domain: exec.domain.clone(),
            log: exec.log.clone(),
            supervisor: exec.supervisor.clone(),
            state: exec.state.clone(),
            options: exec.options.clone(),
        }
    }
}

impl From<ModRefinement> for LModule {
    fn from(m: ModRefinement) -> Self {
        let mut module = LModule::new(m, MOD_REFINEMENT, DOC_MOD_REFINEMENT);
        module.add_async_fn(REFINE, refine, DOC_REFINE, false);
        module.add_async_fn(SET_SUCCESS, set_success, DOC_SET_SUCCESS, false);
        module.add_async_fn(_RETRY, _retry, DOC__RETRY, false);

        //Lambdas
        module.add_lambda(EXEC_TASK, LAMBDA_EXEC_TASK, DOC_EXEC_TASK);
        module.add_lambda(RETRY, LAMBDA_RETRY, DOC_RETRY);
        module.add_lambda(
            __GET_PRECONDITIONS__,
            LAMBDA___GET_PRECONDITIONS__,
            DOC___GET_PRECONDITIONS__,
        );
        module.add_lambda(__GET_SCORE__, LAMBDA___GET_SCORE__, DOC___GET_SCORE__);
        module.add_lambda(
            __GET_COMMAND_MODEL__,
            LAMBDA___GET_COMMAND_MODEL__,
            DOC___GET_COMMAND_MODEL__,
        );
        module.add_lambda(
            __EVAL_PRE_CONDITIONS__,
            LAMBDA___EVAL_PRE_CONDITIONS__,
            DOC___EVAL_PRE_CONDITIONS__,
        );
        module.add_lambda(
            __COMPUTE_SCORE__,
            LAMBDA___COMPUTE_SCORE__,
            DOC___COMPUTE_SCORE__,
        );
        module.add_lambda(IS_APPLICABLE, LAMBDA_IS_APPLICABLE, DOC_IS_APPLICABLE);
        module.add_lambda(
            __GENERATE_APPLICABLE_INSTANCES__,
            LAMBDA___GENERATE_APPLICABLE_INSTANCES__,
            DOC___GENERATE_APPLICABLE_INSTANCES__,
        );
        module.add_lambda(
            __R_GENERATE_INSTANCES__,
            LAMBDA___R_GENERATE_INSTANCES__,
            DOC___R_GENERATE_INSTANCES__,
        );
        module.add_lambda(
            __R_TEST_METHOD__,
            LAMBDA_R_TEST_METHOD,
            DOC___R_TEST_METHOD__,
        );

        module
    }
}

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task: LValue = args.into();
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let supervisor = &ctx.supervisor;
    let log = ctx.log.clone();

    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)
        .unwrap()
        .process_ref;
    let task_id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if supervisor.get_kind(*id).await.unwrap() == ProcessKind::Method {
                supervisor
                    .new_task(
                        MethodLabel::Subtask(supervisor.get_number_subtask(*id).await),
                        *id,
                        task.clone(),
                        false,
                    )
                    .await
            } else {
                panic!()
            }
        }
        ProcessRef::Relative(id, labels) => match supervisor.get_id(pr.clone()).await {
            Some(id) => id,
            None => match labels[0] {
                Label::MethodProcess(MethodLabel::Subtask(s)) => {
                    supervisor
                        .new_task(MethodLabel::Subtask(s), *id, task.clone(), false)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let sr: SelectResponse = select(task_id, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    let result: LValue = match sr {
        SelectResponse::Planned(id) => {
            let mut inner = ctx.supervisor.inner.write().await;

            let task_process: &mut TaskProcess =
                inner.get_mut(task_id).unwrap().inner.as_mut_task().unwrap();
            task_process.set_status(ActionStatus::Running(None));

            drop(task_process);
            let method: &mut MethodProcess =
                inner.get_mut(id).unwrap().inner.as_mut_method().unwrap();

            method.set_start(supervisor.get_timepoint());
            method.status = ActionStatus::Running(None);
            let value = method.value.clone();
            let value_as_slice: Vec<LValue> = value.try_into()?;
            let label = value_as_slice[0].to_string();
            let params_values = &value_as_slice[1..];
            let expanded = method.expanded.clone();
            let mut body = vec![
                LPrimitive::Begin.into(),
                list!(DEF_PROCESS_ID.into(), id.into()),
            ];

            let labels: Vec<Arc<Sym>> = ctx
                .domain
                .read()
                .await
                .get_methods()
                .get(&label)
                .unwrap()
                .parameters
                .get_labels();

            for (param, value) in labels.iter().zip(params_values) {
                body.push(list![
                    LPrimitive::Define.into(),
                    param.into(),
                    value.clone()
                ]);
            }

            body.push(expanded);
            list!(body.into(), task_id.into())
        }
        SelectResponse::Generated(r) => {
            let method = r.method_value.clone();

            let instant = ctx.supervisor.get_timepoint();

            let mut inner = ctx.supervisor.inner.write().await;

            let task_process: &mut TaskProcess =
                inner.get_mut(task_id).unwrap().inner.as_mut_task().unwrap();

            if method == LValue::Nil {
                log.error(format!("No applicable method for task {task}({task_id})"))
                    .await;
                task_process.set_status(ActionStatus::Failure);
                task_process.set_end(instant);
                RaeExecError::NoApplicableMethod.into()
            } else {
                let debug = method.to_string();
                let mut p_env = PLEnv {
                    env: env.clone(),
                    pc: Default::default(),
                    unpure_bindings: Default::default(),
                };
                let (label, params_values) = if let LValue::List(method) = &method {
                    (method[0].to_string(), method[1..].to_vec())
                } else {
                    panic!()
                };
                let p_method: LValue = p_eval(&method, &mut p_env).await?;
                let p_method: LValue = annotate(p_method);
                log.debug(format!("p_eval({}) => {}", debug, method)).await;
                task_process.set_status(ActionStatus::Running(None));
                drop(task_process);
                let id: ActingProcessId = inner.new_method(task_id, debug, r, false);
                let mut body = vec![
                    LPrimitive::Begin.into(),
                    list!(DEF_PROCESS_ID.into(), id.into()),
                ];

                let labels: Vec<Arc<Sym>> = ctx
                    .domain
                    .read()
                    .await
                    .get_methods()
                    .get(&label)
                    .unwrap()
                    .parameters
                    .get_labels();

                for (param, value) in labels.iter().zip(params_values) {
                    body.push(list![LPrimitive::Define.into(), param.into(), value]);
                }

                body.push(p_method);
                list!(body.into(), task_id.into())
            }
        }
    };

    Ok(result)
}

#[async_scheme_fn]
pub async fn set_success(env: &LEnv) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let task_id = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();

    let supervisor = &env.get_context::<ModRefinement>(MOD_REFINEMENT)?.supervisor;
    let instant = supervisor.get_timepoint();
    let mut inner = supervisor.inner.write().await;
    let task: &mut TaskProcess = inner.get_mut(task_id).unwrap().inner.as_mut_task().unwrap();

    task.set_status(ActionStatus::Success);
    task.set_end(instant);

    drop(task);
    drop(inner);

    supervisor
        .set_task_last_refinement_end(&task_id, ActionStatus::Success)
        .await;

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn _retry(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let supervisor = &env.get_context::<ModExec>(MOD_EXEC)?.supervisor;
    let task_id = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();
    let log = ctx.log.clone();
    supervisor
        .set_task_last_refinement_end(&task_id, ActionStatus::Failure)
        .await;
    let task: LValue = ctx
        .supervisor
        .inner
        .read()
        .await
        .get(task_id)
        .unwrap()
        .inner
        .as_task()
        .unwrap()
        .value
        .clone();

    log.error(format!("Retrying task {task}({task_id})")).await;

    let sr: SelectResponse = select(task_id, env).await?;

    let result: LValue = match sr {
        SelectResponse::Planned(id) => {
            let mut inner = ctx.supervisor.inner.write().await;

            let task_process: &mut TaskProcess =
                inner.get_mut(task_id).unwrap().inner.as_mut_task().unwrap();
            task_process.set_status(ActionStatus::Running(None));

            drop(task_process);
            let method: &mut MethodProcess =
                inner.get_mut(id).unwrap().inner.as_mut_method().unwrap();

            method.set_start(supervisor.get_timepoint());
            method.status = ActionStatus::Running(None);
            let value = method.value.clone();
            let value_as_slice: Vec<LValue> = value.try_into()?;
            let label = value_as_slice[0].to_string();
            let params_values = &value_as_slice[1..];
            let expanded = method.expanded.clone();
            let mut body = vec![
                LPrimitive::Begin.into(),
                list!(DEF_PROCESS_ID.into(), id.into()),
            ];

            let labels: Vec<Arc<Sym>> = ctx
                .domain
                .read()
                .await
                .get_methods()
                .get(&label)
                .unwrap()
                .parameters
                .get_labels();

            for (param, value) in labels.iter().zip(params_values) {
                body.push(list![
                    LPrimitive::Define.into(),
                    param.into(),
                    value.clone()
                ]);
            }

            body.push(expanded);
            list!(body.into(), task_id.into())
        }
        SelectResponse::Generated(r) => {
            let new_method = r.method_value.clone();

            let instant = ctx.supervisor.get_timepoint();

            let mut inner = ctx.supervisor.inner.write().await;
            let tp: &mut TaskProcess = inner.get_mut(task_id).unwrap().inner.as_mut_task().unwrap();

            if new_method == LValue::Nil {
                log.error(format!(
                    "No more method for task {task}({task_id}). Task is a failure!",
                ))
                .await;
                tp.set_status(ActionStatus::Failure);
                tp.set_end(instant);
                RaeExecError::NoApplicableMethod.into()
            } else {
                drop(tp);
                let debug = new_method.to_string();
                /*let new_method: LValue = p_eval(&new_method, &mut env.clone(), &mut PConfig::default())
                .await
                .unwrap_or_else(|e| panic!("{e}"))
                .get_lvalue()
                .clone();*/
                log.debug(format!("p_eval({}) => {}", debug, new_method))
                    .await;
                let id: ActingProcessId = ctx.supervisor.new_method(task_id, debug, r, false).await;
                list!(
                    LPrimitive::Begin.into(),
                    list!(DEF_PROCESS_ID.into(), id.into()),
                    list!(LPrimitive::Enr.into(), new_method)
                )
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

pub enum SelectResponse {
    Planned(ActingProcessId),
    Generated(RefinementInner),
}

pub async fn select(task_id: ActingProcessId, env: &LEnv) -> Result<SelectResponse, LRuntimeError> {
    /*
    Each function return an ordered list of methods
     */
    let mod_refinement = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let log = mod_refinement.log.clone();
    let supervisor = &mod_refinement.supervisor;
    let task: LValue = supervisor.get_task_value(&task_id).await;
    let state: WorldStateSnapshot = mod_refinement.state.get_snapshot().await;
    let select_mode: SelectMode = *mod_refinement.options.read().await.get_select_mode();

    let task_slice: Vec<LValue> = task.clone().try_into()?;
    let tried: Vec<LValue> = supervisor.get_tried_method(task_id).await;

    let greedy_refinement: RefinementInner = greedy_select(&state, &tried, task_slice, env)
        .await
        .map_err(|e| e.chain("greedy_select"))?;

    let planned_refinement: Option<Refinement> =
        supervisor.get_task_planned_refinement(&task_id).await;

    match planned_refinement {
        Some(mut refinement) => {
            if greedy_refinement
                .possibilities
                .contains(&refinement.inner.method_value)
            {
                refinement.inner.possibilities = greedy_refinement.possibilities;
                refinement.inner.interval = greedy_refinement.interval;
                refinement.inner.select = SelectTrace::ContinuousPlanning;
                let method_id = refinement.method_id;
                supervisor
                    .update_task_last_refinement(&task_id, refinement)
                    .await;
                Ok(SelectResponse::Planned(method_id))
            } else {
                Ok(SelectResponse::Generated(greedy_refinement))
            }
        }
        None => {
            let rt: RefinementInner = match &select_mode {
                SelectMode::Greedy => {
                    /*
                    Returns all applicable methods sorted by their score
                     */

                    log.debug(format!("select greedy for {task}")).await;
                    greedy_refinement
                }
                SelectMode::Planning(Planner::Aries(bool)) => {
                    log.debug(format!("select with aries for {task}")).await;
                    aries_select(&state, greedy_refinement, env, *bool)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::CChoice(config)) => {
                    log.debug(format!("select with c-choice for {task}")).await;
                    c_choice_select(&state, greedy_refinement, env, *config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::RAEPlan(config)) => {
                    log.debug(format!("select with c-choice for {task}")).await;
                    rae_plan_select(&state, greedy_refinement, env, *config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                _ => todo!(),
            };

            log.debug(format!(
                "Sorted_methods for {}({}): {}",
                task,
                task_id,
                LValue::from(&rt.possibilities)
            ))
            .await;

            Ok(SelectResponse::Generated(rt))
        }
    }
}

pub async fn greedy_select(
    state: &WorldStateSnapshot,
    tried: &Vec<LValue>,
    task: Vec<LValue>,
    env: &LEnv,
) -> lruntimeerror::Result<RefinementInner> {
    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (best_method, task_id)
     */
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let time = env.get_context::<ModTime>(MOD_TIME)?;

    //println!("task to test in greedy: {}", LValue::from(task.clone()));
    let start = time.get_micros().await;

    let task_label = task[0].to_string();
    //let task_string = LValue::from(task.clone()).to_string();
    let params: Vec<LValue> = task[1..].iter().map(|lv| list![lv.clone()]).collect();

    let mut applicable_methods: Vec<(LValue, i64)> = vec![];

    let mut env = env.clone();

    env.update_context(ModState::new_from_snapshot(state.clone()));
    let env = &env;

    let domain: OMPASDomain = ctx.domain.read().await.clone();

    let method_templates: Vec<String> =
        domain.tasks.get(&task_label).unwrap().get_methods().clone();

    for template in &method_templates {
        let method_template = domain.methods.get(template).unwrap();
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
                &list!(LPrimitive::Enr.into(), list!(LPrimitive::Quote.into(), arg)),
                &mut env.clone(),
                None,
            )
            .await
            .map_err(|e: LRuntimeError| e.chain(format!("eval pre_conditions: {}", arg_debug)))?;
            if !matches!(lv, LValue::Err(_)) {
                let arg = cons(env, &[score_lambda.clone(), i_vec[1..].into()])?;
                let arg_debug = arg.to_string();
                let score: i64 = eval(
                    &list!(LPrimitive::Enr.into(), list!(LPrimitive::Quote.into(), arg)),
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

    Ok(RefinementInner {
        task_value: task.into(),
        method_value: choosed,
        interval: Interval::new(start, Some(ctx.supervisor.get_timepoint())),
        possibilities: methods,
        select: SelectTrace::RealTime(RTSelect {
            refinement_type: SelectMode::Greedy,
        }),
        tried: tried.to_vec(),
    })
}
