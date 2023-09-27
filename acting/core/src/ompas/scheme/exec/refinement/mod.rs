pub mod aries;
pub mod c_choice;
pub mod rae_plan;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::error::RaeExecError;
use crate::ompas::interface::select_mode::{Planner, SelectMode};
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::process::task::{RefinementTrace, Selected};
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId, MethodModel};
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::ompas::OMPASManager;
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use crate::ompas::scheme::exec::refinement::aries::aries_select;
use crate::ompas::scheme::exec::refinement::c_choice::c_choice_select;
use crate::ompas::scheme::exec::refinement::rae_plan::rae_plan_select;
use crate::ompas::scheme::exec::state::{instances, ModState};
use crate::ompas::scheme::exec::ModExec;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::refinement::*;
use ompas_language::exec::MOD_EXEC;
use ompas_middleware::logger::LogClient;
use rand::prelude::SliceRandom;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_macros::async_scheme_fn;
use sompas_modules::utils::enumerate;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror};
use std::borrow::Borrow;
use std::convert::TryInto;
use std::time::Duration;
use tokio::time::sleep;

pub struct ModRefinement {
    pub domain: DomainManager,
    pub acting_manager: ActingManager,
    pub options: OMPASManager,
    pub clock_manager: ClockManager,
    pub log: LogClient,
}

impl ModRefinement {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            domain: exec.domain.clone(),
            log: exec.log.clone(),
            acting_manager: exec.acting_manager.clone(),
            options: exec.options.clone(),
            clock_manager: exec.acting_manager.clock_manager.clone(),
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
        /*module.add_lambda(__GET_SCORE__, LAMBDA___GET_SCORE__, DOC___GET_SCORE__);
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
        );*/

        module
    }
}

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task: LValue = args.into();
    let debug = task.to_string();
    let mut args = args.iter().map(|lv| lv.as_cst()).collect();
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let acting_manager = &ctx.acting_manager;

    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)
        .unwrap()
        .process_ref;
    let task_id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if acting_manager.get_kind(id).await == ProcessKind::Method {
                acting_manager
                    .new_task(
                        Label::Task(acting_manager.get_number_subtask(*id).await),
                        id,
                        args,
                        debug,
                        ProcessOrigin::Execution,
                    )
                    .await
            } else {
                panic!()
            }
        }
        ProcessRef::Relative(id, labels) => match acting_manager.get_id(pr.clone()).await {
            Some(id) => {
                acting_manager
                    .set_action_args(&id, args.drain(..).map(|c| c.unwrap()).collect())
                    .await;
                id
            }
            None => match labels[0] {
                Label::Task(s) => {
                    acting_manager
                        .new_task(Label::Task(s), id, args, debug, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let log = ctx.log.clone();
    let debug = acting_manager.get_debug(&task_id).await.unwrap();
    log.debug(format!("({task_id}) Refine {debug} "));

    acting_manager.set_start(&task_id, None).await;

    let rt: RefinementTrace = select(task_id, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    check_refinement_trace(env, ctx, &task_id, rt)
        .await
        .map_err(|e| e.chain("check_refinement_trace"))
}

async fn check_refinement_trace(
    env: &LEnv,
    ctx: &ModRefinement,
    task_id: &ActingProcessId,
    mut rt: RefinementTrace,
) -> LResult {
    let log = ctx.log.clone();
    let acting_manager = &ctx.acting_manager;
    let debug = acting_manager.get_debug(task_id).await.unwrap();
    let method_id = match &rt.selected {
        Selected::Anticipated(refinement_id) => {
            let method_debug = acting_manager.get_debug(&refinement_id).await.unwrap();
            log.debug(format!(
                "({task_id}) Chose planned refinement for {debug}: ({refinement_id}) {method_debug}"
            ));
            *refinement_id
        }
        Selected::Generated(method, _) => {
            if method == &LValue::Nil {
                log.error(format!(
                    "({task_id}) No applicable method for task {debug}({task_id})"
                ));
                acting_manager
                    .set_end(task_id, None, ProcessStatus::Failure)
                    .await;
                return Ok(RaeExecError::NoApplicableMethod.into());
            } else {
                let debug = method.to_string();
                let p_env = PLEnv {
                    env: env.clone(),
                    pc: Default::default(),
                    unpure_bindings: Default::default(),
                };

                let args = if let LValue::List(list) = &method {
                    list.iter().map(|lv| lv.as_cst()).collect()
                } else {
                    panic!()
                };

                acting_manager
                    .new_executed_method(
                        task_id,
                        debug,
                        args,
                        MethodModel::Raw(method.clone(), p_env),
                        ProcessOrigin::Execution,
                    )
                    .await
            }
        }
    };

    rt.duration.set_end(acting_manager.clock_manager.now());

    acting_manager
        .set_executed_refinement(task_id, &method_id, rt)
        .await;

    let program = acting_manager.get_om_lvalue(&method_id).await;

    log.debug(format!("({method_id}) program: \n{}", program.format(0)));

    Ok(program)
}

#[async_scheme_fn]
pub async fn set_success(env: &LEnv) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;

    let task_id = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();

    let acting_manager = &env
        .get_context::<ModRefinement>(MOD_REFINEMENT)?
        .acting_manager;

    ctx.log.debug(format!("({task_id}) Success"));

    acting_manager
        .set_end(&task_id, None, ProcessStatus::Success)
        .await;

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn _retry(env: &LEnv, err: LValue) -> LResult {
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let acting_manager = &env.get_context::<ModExec>(MOD_EXEC)?.acting_manager;
    let task_id = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();
    let debug = acting_manager.get_debug(&task_id).await.unwrap();
    let log = ctx.log.clone();
    log.error(format!("({task_id}) Failed {debug}: {}", err));
    if let Some(refinement_id) = acting_manager.get_last_executed_refinement(&task_id).await {
        acting_manager.set_failed_method(&refinement_id).await;
        let rt: RefinementTrace = select(task_id, env).await?;
        check_refinement_trace(env, ctx, &task_id, rt).await
    } else {
        println!(
            "Attempt retry when there were no previous refinement for {}",
            task_id
        );
        acting_manager.dump_trace(None).await;
        sleep(Duration::from_secs(1)).await;
        std::process::exit(0)
    }
}

pub async fn select(
    task_id: ActingProcessId,
    env: &LEnv,
) -> Result<RefinementTrace, LRuntimeError> {
    /*
    Each function return an ordered list of methods
     */
    let mod_refinement = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let duration = Interval::new_instant(mod_refinement.clock_manager.now());
    let log = mod_refinement.log.clone();
    let acting_manager = &mod_refinement.acting_manager;
    let task: Vec<LValue> = acting_manager
        .get_task_args(&task_id)
        .await
        .iter()
        .map(|cst| LValue::from(cst.clone()))
        .collect();
    let state: WorldStateSnapshot = mod_refinement.acting_manager.state.get_snapshot().await;

    let mut candidates = applicable(&state, &task, env).await?;

    let tried: Vec<LValue> = acting_manager.get_tried(&task_id).await;

    candidates.retain(|m| !tried.contains(m));

    log.debug(format!(
        "({}) Candidates = {}",
        task_id,
        LValue::from(&candidates),
    ));

    let planned_refinement: Option<ActingProcessId> =
        acting_manager.get_last_planned_refinement(&task_id).await;

    let mut selected = if let Some(refinement) = planned_refinement {
        let lv: LValue = acting_manager.get_refinement_lv(&refinement).await;
        log.debug(format!(
            "({task_id}) Select found planned refinement: ({refinement}) {};",
            lv,
        ));
        if candidates.contains(&lv) {
            log.debug(format!(
                "({task_id}) Valid planned refinement ({refinement}) {lv}"
            ));
            Some(Selected::Anticipated(refinement))
        } else {
            log.warn(format!(
                "({task_id}) Unvalid planned refinement ({refinement}) {lv}"
            ));
            None
        }
    } else {
        None
    };

    selected = if selected.is_none() {
        let select_mode: SelectMode = mod_refinement.options.get_select_mode().await;

        Some(Selected::Generated(
            match select_mode {
                SelectMode::Greedy => {
                    /*
                    Returns all applicable methods sorted by their score
                     */
                    greedy_select(&candidates, &state, env)?
                }
                SelectMode::Random => random_select(&candidates, &state, env)?,
                SelectMode::Score => score_select(&candidates, &state, env).await?,
                SelectMode::Planning(Planner::Aries(bool)) => {
                    aries_select(&candidates, &state, env, bool)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::CChoice(config)) => {
                    c_choice_select(&candidates, &state, env, config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::RAEPlan(config)) => {
                    rae_plan_select(&candidates, &state, env, config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Heuristic
                | SelectMode::Learning
                | SelectMode::Planning(Planner::UPOM) => {
                    todo!()
                }
            },
            select_mode,
        ))
    } else {
        selected
    };

    let rt = RefinementTrace {
        selected: selected.unwrap(),
        candidates,
        duration,
    };

    log.debug(format!("({task_id}) selected: {}", rt.selected));

    Ok(rt)
}

pub async fn candidates(
    state: &WorldStateSnapshot,
    tried: &[LValue],
    task: &[LValue],
    env: &LEnv,
) -> lruntimeerror::Result<Vec<LValue>> {
    let mut applicable = applicable(state, task, env).await?;
    applicable.retain(|m| !tried.contains(m));
    Ok(applicable)
}

pub async fn applicable(
    state: &WorldStateSnapshot,
    task: &[LValue],
    env: &LEnv,
) -> lruntimeerror::Result<Vec<LValue>> {
    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (best_method, task_id)
     */
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;

    let task_label = task[0].to_string();
    //let task_string = LValue::from(task.clone()).to_string();
    let params: Vec<LValue> = task[1..].iter().map(|lv| list![lv.clone()]).collect();

    let mut applicable_methods: Vec<LValue> = vec![];

    let mut env = env.clone();

    env.update_context(ModState::new_from_snapshot(state.clone()));
    let env = &env;

    let domain: OMPASDomain = ctx.domain.get_inner().await;

    let method_templates: Vec<String> =
        domain.tasks.get(&task_label).unwrap().get_methods().clone();

    for template in &method_templates {
        let method_template = domain.methods.get(template).unwrap();
        let types: Vec<LValue> = method_template
            .parameters
            .get_types_as_lvalue()
            .try_into()?;

        let pre_conditions_lambda = method_template.lambda_pre_conditions.clone();

        let mut instances_template = vec![template.into()];
        instances_template.append(&mut params.clone());

        for t in &types[params.len()..] {
            instances_template.push(instances(env, &[t.clone()]).await?);
        }

        let mut instances_template: Vec<LValue> =
            enumerate(env, &instances_template)?.try_into()?;

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
                applicable_methods.push(i);
            }
        }
    }

    Ok(applicable_methods)
}

pub async fn score_select(
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
) -> lruntimeerror::Result<LValue> {
    let mut tuple: Vec<(&LValue, i64)> = vec![];

    let domain = env.get_context::<ModExec>(MOD_EXEC)?.domain.clone();

    let mut env = env.clone();
    env.update_context(ModState::new_from_snapshot(state.clone()));

    for candidate in candidates {
        let LValue::List(list) = &candidate else {
            unreachable!()
        };

        let score_lambda = domain
            .get_method(&list[0].to_string())
            .await
            .unwrap()
            .lambda_score
            .clone();

        let arg = cons(&env, &[score_lambda, list[1..].into()])?;
        let arg_debug = arg.to_string();
        let score: i64 = eval(
            &list!(LPrimitive::Enr.into(), list!(LPrimitive::Quote.into(), arg)),
            &mut env.clone(),
            None,
        )
        .await
        .map_err(|e: LRuntimeError| e.chain(format!("eval score: {}", arg_debug)))?
        .try_into()?;

        tuple.push((candidate, score));
    }

    tuple.sort_by_key(|(_, s)| *s);

    Ok(tuple.last().map(|e| e.0.clone()).unwrap())
}

pub fn random_select(
    candidates: &[LValue],
    _: &WorldStateSnapshot,
    _: &LEnv,
) -> lruntimeerror::Result<LValue> {
    let mut rng = rand::thread_rng();
    let mut candidates: Vec<&LValue> = candidates.iter().map(|lv| lv).collect();
    candidates.shuffle(&mut rng);
    Ok(candidates.get(0).cloned().unwrap_or(&LValue::Nil).clone())
}

pub fn greedy_select(
    candidates: &[LValue],
    _: &WorldStateSnapshot,
    _: &LEnv,
) -> lruntimeerror::Result<LValue> {
    Ok(candidates.get(0).cloned().unwrap_or(LValue::Nil))
}
