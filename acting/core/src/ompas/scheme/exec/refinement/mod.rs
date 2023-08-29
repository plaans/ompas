pub mod aries;
pub mod c_choice;
pub mod rae_plan;
use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::error::RaeExecError;
use crate::ompas::interface::select_mode::{Planner, SelectMode};
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::process::task::{RTSelect, RefinementInner, SelectTrace};
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId};
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::ompas::OMPASManager;
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use crate::ompas::scheme::exec::refinement::aries::aries_select;
use crate::ompas::scheme::exec::refinement::c_choice::c_choice_select;
use crate::ompas::scheme::exec::refinement::rae_plan::rae_plan_select;
use crate::ompas::scheme::exec::state::{instance, ModState};
use crate::ompas::scheme::exec::ModExec;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::refinement::*;
use ompas_language::exec::MOD_EXEC;
use ompas_middleware::logger::LogClient;
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
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror};
use std::borrow::Borrow;
use std::convert::TryInto;

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
                    .new_action(
                        Label::Action(acting_manager.get_number_subtask(*id).await),
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
                Label::Action(s) => {
                    acting_manager
                        .new_action(Label::Action(s), id, args, debug, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    acting_manager.set_start(&task_id, None).await;
    let sr: SelectResponse = select(task_id, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    check_select_response(env, ctx, &task_id, sr)
        .await
        .map_err(|e| e.chain("check_select_response"))
}

async fn check_select_response(
    env: &LEnv,
    ctx: &ModRefinement,
    task_id: &ActingProcessId,
    sr: SelectResponse,
) -> LResult {
    let log = ctx.log.clone();
    let acting_manager = &ctx.acting_manager;
    let debug = acting_manager.get_debug(task_id).await.unwrap();
    let method_id = match sr {
        SelectResponse::Planned(refinement_id) => {
            log.debug(format!("Planned refinement for {debug}")).await;
            refinement_id
        }
        SelectResponse::Generated(r) => {
            let method = r.method_value.clone();

            if method == LValue::Nil {
                log.error(format!("No applicable method for task {debug}({task_id})"))
                    .await;
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

                //println!("generating refinement for {}", method);
                let model: ActingModel = acting_manager
                    .generate_acting_model_for_method(&method, p_env)
                    .await?;

                //println!("refinement args: {}", &method);

                let args = if let LValue::List(list) = method {
                    list.iter().map(|lv| lv.as_cst()).collect()
                } else {
                    panic!()
                };

                acting_manager
                    .new_refinement(task_id, debug, args, model, ProcessOrigin::Execution, None)
                    .await
            }
        }
    };

    acting_manager
        .set_status(task_id, ProcessStatus::Running(None))
        .await;
    acting_manager.set_start(&method_id, None).await;
    acting_manager
        .set_status(&method_id, ProcessStatus::Running(None))
        .await;

    let program = acting_manager.get_om_lvalue(&method_id).await;
    Ok(program)
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

    let supervisor = &env
        .get_context::<ModRefinement>(MOD_REFINEMENT)?
        .acting_manager;
    supervisor
        .set_end(&task_id, None, ProcessStatus::Success)
        .await;

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn _retry(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let supervisor = &env.get_context::<ModExec>(MOD_EXEC)?.acting_manager;
    let task_id = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();
    let log = ctx.log.clone();
    let refinement_id: ActingProcessId = supervisor
        .get_last_executed_refinement(&task_id)
        .await
        .unwrap();
    supervisor
        .set_end(&refinement_id, None, ProcessStatus::Failure)
        .await;
    let debug = supervisor.get_debug(&task_id).await.unwrap();

    log.error(format!("Retrying task {debug}({task_id})")).await;

    let sr: SelectResponse = select(task_id, env).await?;

    check_select_response(env, ctx, &task_id, sr).await
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
    let supervisor = &mod_refinement.acting_manager;
    let task: Vec<LValue> = supervisor
        .get_task_args(&task_id)
        .await
        .iter()
        .map(|cst| LValue::from(cst.clone()))
        .collect();
    let debug: String = supervisor.get_debug(&task_id).await.unwrap();
    let state: WorldStateSnapshot = mod_refinement.acting_manager.state.get_snapshot().await;
    let select_mode: SelectMode = mod_refinement.options.get_select_mode().await;

    let tried: Vec<LValue> = supervisor.get_tried(&task_id).await;

    let greedy_refinement: RefinementInner = greedy_select(&state, &tried, &task, env)
        .await
        .map_err(|e| e.chain("greedy_select"))?;

    let planned_refinement: Option<ActingProcessId> =
        supervisor.get_last_planned_refinement(&task_id).await;

    match planned_refinement {
        Some(refinement) => {
            let lv: LValue = supervisor.get_refinement_lv(&refinement).await;
            let possibilities = &greedy_refinement.possibilities;
            println!(
                "{}.refinement = planned({}) = {}\napplicable = {}",
                task_id,
                refinement,
                lv,
                LValue::from(possibilities.clone())
            );
            if possibilities.contains(&lv) {
                println!("selecting planned refinement!");
                Ok(SelectResponse::Planned(refinement))
            } else {
                Ok(SelectResponse::Generated(greedy_refinement))
            }
        }
        None => {
            let rt: RefinementInner = match select_mode {
                SelectMode::Greedy => {
                    /*
                    Returns all applicable methods sorted by their score
                     */

                    log.debug(format!("select greedy for {debug}")).await;
                    greedy_refinement
                }
                SelectMode::Planning(Planner::Aries(bool)) => {
                    log.debug(format!("select with aries for {debug}")).await;
                    aries_select(&state, greedy_refinement, env, bool)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::CChoice(config)) => {
                    log.debug(format!("select with c-choice for {debug}")).await;
                    c_choice_select(&state, greedy_refinement, env, config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                SelectMode::Planning(Planner::RAEPlan(config)) => {
                    log.debug(format!("select with c-choice for {debug}")).await;
                    rae_plan_select(&state, greedy_refinement, env, config)
                        .await
                        .map_err(|e| e.chain("planning_select"))?
                }
                _ => todo!(),
            };

            log.debug(format!(
                "Sorted_methods for {}({}): {}",
                debug,
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
    tried: &[LValue],
    task: &[LValue],
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

    let domain: OMPASDomain = ctx.domain.get_inner().await;

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
    let (methods, chosen) = {
        let mut rng = rand::thread_rng();
        applicable_methods.shuffle(&mut rng);
        applicable_methods.sort_by_key(|a| a.1);
        applicable_methods.reverse();
        let mut methods: Vec<_> = applicable_methods.drain(..).map(|a| a.0).collect();
        methods.retain(|m| !tried.contains(m));
        let chosen = methods.get(0).cloned().unwrap_or(LValue::Nil);
        (methods, chosen)
    };
    let interval = Interval::new(start, Some(ctx.clock_manager.now()));
    Ok(RefinementInner {
        task_value: task.into(),
        method_value: chosen,
        interval,
        possibilities: methods,
        select: SelectTrace::RealTime(RTSelect {
            refinement_type: SelectMode::Greedy,
        }),
        tried: tried.to_vec(),
    })
}
