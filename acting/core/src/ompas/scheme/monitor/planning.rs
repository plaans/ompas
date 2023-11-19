use crate::model::acting_domain::model::{Event, Goal, NewTask};
use crate::model::acting_domain::OMPASDomain;
use crate::model::add_domain_symbols;
use crate::model::chronicle::Instantiation;
use crate::model::sym_domain::cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::planning::{extract_choices, DebugDate};
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::ompas::scheme::monitor::ModMonitor;
use crate::planning::planner::ompas_lcp;
use crate::planning::planner::ompas_lcp::OMPASLCPConfig;
use crate::planning::planner::problem::new_problem_chronicle_instance;
use crate::planning::planner::result::instance::instantiate_chronicles;
use crate::planning::planner::solver::PMetric;
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG, OMPAS_PLAN_OUTPUT};
use aries_planners::solver::SolverResult;
use ompas_language::monitor::control::MOD_CONTROL;
use ompas_language::monitor::model::MOD_MODEL;
use ompas_language::monitor::planning::*;
use sompas_macros::async_scheme_fn;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Clone)]
pub struct ModPlanning {
    events: Arc<RwLock<Vec<Event>>>,
    goals: Arc<RwLock<Vec<Goal>>>,
    tasks: Arc<RwLock<Vec<NewTask>>>,
    _st: RefSymTable,
    _domain: DomainManager,
}

impl ModPlanning {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            events: Arc::new(Default::default()),
            goals: Arc::new(Default::default()),
            tasks: Arc::new(Default::default()),
            _st: monitor.acting_manager.st.clone(),
            _domain: monitor.acting_manager.domain_manager.clone(),
        }
    }
}

impl From<ModPlanning> for LModule {
    fn from(m: ModPlanning) -> Self {
        let mut m = LModule::new(m, MOD_PLANNING, DOC_MOD_PLANNING);
        m.add_async_fn(PLAN, plan, DOC_PLAN, false);
        m.add_async_fn(PLAN_OPT, plan_opt, DOC_PLAN_OPT, false);
        m.add_async_fn(
            NEW_GOAL_TASK,
            new_goal_task,
            (DOC_NEW_GOAL_TASK, DOC_NEW_GOAL_TASK_VERBOSE),
            false,
        );
        m.add_async_fn(
            NEW_TIMED_GOAL_TASK,
            new_timed_goal_task,
            (DOC_NEW_TIMED_GOAL_TASK, DOC_NEW_TIMED_GOAL_TASK_VERBOSE),
            false,
        );
        m.add_async_fn(
            NEW_EVENT,
            new_event,
            (DOC_NEW_EVENT, DOC_NEW_EVENT_VERBOSE),
            false,
        );
        m.add_async_fn(
            NEW_GOAL,
            new_goal,
            (DOC_NEW_GOAL, DOC_NEW_GOAL_VERBOSE),
            false,
        );
        m.add_async_fn(
            GET_GOALS_EVENTS,
            get_goals_events,
            DOC_GET_GOALS_EVENTS,
            false,
        );
        m.add_async_fn(REMOVE_GOAL, remove_goal, DOC_REMOVE_GOAL, false);
        m.add_async_fn(REMOVE_TASK, remove_task, DOC_REMOVE_TASK, false);
        m.add_async_fn(REMOVE_EVENT, remove_event, DOC_REMOVE_EVENT, false);
        m
    }
}

async fn _plan(env: &LEnv, task: &[LValue], opt: bool) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let mut tasks = ctx.tasks.read().await.clone();
    if !task.is_empty() {
        tasks.push(NewTask {
            start: None,
            args: read_slice(task).map_err(|e| e.chain("_plan_task"))?,
        });
    }
    let goals = ctx.goals.read().await.clone();
    let events = ctx.events.read().await.clone();

    __plan(env, opt, tasks, goals, events).await
}

/// Plan a task
#[async_scheme_fn]
pub async fn plan(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    _plan(env, args, false).await
}

#[async_scheme_fn]
pub async fn plan_opt(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    _plan(env, args, true).await
}

///Plan the problem as defined in the context
pub async fn __plan(
    env: &LEnv,
    opt: bool,
    tasks: Vec<NewTask>,
    goals: Vec<Goal>,
    events: Vec<Event>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let pre_compute_models = ctx.options.get_pre_compute_models().await;

    let acting_manager = ctx.acting_manager.clone();
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let mut env: LEnv = ctx.get_plan_env().await;
    let state: WorldStateSnapshot = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state.clone()));
    let opt = if opt { Some(PMetric::Makespan) } else { None };
    let st = acting_manager.st.clone();
    acting_manager
        .domain_manager
        .init_planning_domain(&env, state, &st, pre_compute_models)
        .await;

    let domain: OMPASDomain = acting_manager.domain_manager.get_inner().await;
    add_domain_symbols(&st, &domain);

    let mut state = acting_manager.state_manager.get_snapshot().await;
    let state_manager = acting_manager.state_manager.clone();
    let resource_state = acting_manager.resource_manager.get_snapshot(None).await;
    state.absorb(resource_state);
    let mut ep: ExecutionProblem = ExecutionProblem {
        state,
        st: st.clone(),
        chronicles: vec![new_problem_chronicle_instance(&st, tasks.clone(), goals, events).await],
    };

    //hack
    for (i, task) in tasks.iter().enumerate() {
        if let Some(start) = task.start {
            let ch = &mut ep.chronicles[i + 1].instantiated_chronicle;
            *ch = ch.clone().instantiate(vec![Instantiation::new(
                ch.interval.get_start(),
                st.new_cst(start.as_cst().unwrap()),
            )]);
        }
    }

    if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
        for (origin, chronicle) in ep
            .chronicles
            .iter()
            .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
        {
            println!("{:?}:{}", origin, chronicle)
        }
    }

    let subscriber = state_manager.new_subscriber(StateRule::All).await;

    let result = ompas_lcp::run_planner(
        &ep,
        &OMPASLCPConfig {
            state_subscriber_id: subscriber.id,
            opt,
            state_manager,
            domain: Arc::new(domain),
            env,
            debug_date: DebugDate::new(0),
        },
        |_, _| {},
        None,
    )
    .await;

    //println!("{}", format_partial_plan(&pb, &x)?);

    if let Ok(SolverResult::Sol(pr)) = result {
        let choices = extract_choices(&pr);

        if OMPAS_PLAN_OUTPUT.get() {
            println!("Plan found");
            if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
                //let PlanResult { ass, fp, .. } = &pr;

                // for v in ass.variables() {
                //     let prez = format!("[{:?}]", ass.presence_literal(v));
                //     let v_str = format!("{v:?}");
                //     print!("{prez:<6}  {v_str:<6} <- {:?}", ass.domain(v));
                //     if let Some(lbl) = fp.model.get_label(v) {
                //         println!("    {lbl:?}");
                //     } else {
                //         println!()
                //     }
                // }

                let instances = instantiate_chronicles(&pr);

                for i in instances {
                    println!("{}", i)
                }
            }
            for choice in &choices {
                println!("{}:{}", choice.process_ref, choice.choice_inner)
            }
        }
    } else {
        println!("No solution found by planner")
    };

    Ok(())
}

pub fn read_slice(slice: &[LValue]) -> Result<Vec<cst::Cst>, LRuntimeError> {
    let mut args = vec![];
    for arg in slice {
        args.push(read_value(arg).map_err(|e| e.chain("read_slice"))?)
    }
    Ok(args)
}

pub fn read_value(lv: &LValue) -> Result<cst::Cst, LRuntimeError> {
    lv.as_cst().ok_or(LRuntimeError::wrong_type(
        "read_value",
        lv,
        KindLValue::Atom,
    ))
}

pub fn read_moment(moment: &LValue) -> Result<Interval, LRuntimeError> {
    Ok(if let LValue::List(list) = &moment {
        if list.len() != 2 {
            return Err(LRuntimeError::wrong_number_of_args(
                "read_moment",
                list.as_slice(),
                2..2,
            ));
        }

        Interval::new(
            f64::try_from(&list[0]).map_err(|e| e.chain("read_moment"))?,
            Some(f64::try_from(&list[1]).map_err(|e| e.chain("read_moment"))?),
        )
    } else {
        let instant: f64 = moment
            .try_into()
            .map_err(|e: LRuntimeError| e.chain("read_moment"))?;
        Interval::new_instant(instant)
    })
}

#[async_scheme_fn]
pub async fn new_timed_goal_task(env: &LEnv, task_args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let start: f64 = task_args.first().unwrap().try_into()?;
    let start = start.into();

    let mut args = vec![];

    for arg in &task_args[1..task_args.len()] {
        if let Some(cst) = arg.as_cst() {
            args.push(cst);
        } else {
            return Err(LRuntimeError::wrong_type(
                NEW_TIMED_GOAL_TASK,
                arg,
                KindLValue::Atom,
            ));
        }
    }

    let task = NewTask {
        start: Some(start),
        args,
    };

    ctx.tasks.write().await.push(task);
    Ok(())
}

#[async_scheme_fn]
pub async fn new_goal_task(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let args = read_slice(args).map_err(|e| e.chain(NEW_GOAL_TASK))?;

    let task = NewTask { start: None, args };

    ctx.tasks.write().await.push(task);
    Ok(())
}

#[async_scheme_fn]
pub async fn new_event(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    if args.len() < 3 {
        return Err(LRuntimeError::wrong_number_of_args(
            NEW_EVENT,
            args,
            3..std::usize::MAX,
        ));
    }

    let interval = read_moment(&args[0])?;
    let sv = read_slice(&args[1..args.len() - 1])?;

    let value = read_value(args.last().unwrap())?;

    let event = Event {
        interval,
        sv,
        value,
    };

    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;
    ctx.events.write().await.push(event);
    Ok(())
}

#[async_scheme_fn]
pub async fn new_goal(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(LRuntimeError::wrong_number_of_args(NEW_GOAL, args, 2..3));
    }

    let sv: Vec<cst::Cst> = if let LValue::List(list) = &args[0] {
        read_slice(list.as_slice()).map_err(|e| e.chain(NEW_GOAL))?
    } else {
        vec![read_value(&args[0]).map_err(|e| e.chain(NEW_GOAL))?]
    };

    let value = read_value(&args[1]).map_err(|e| e.chain(NEW_GOAL))?;

    let interval = if let Some(lv) = args.get(2) {
        Some(read_moment(lv).map_err(|e| e.chain(NEW_GOAL))?)
    } else {
        None
    };

    let goal = Goal {
        interval,
        sv,
        value,
    };

    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;
    ctx.goals.write().await.push(goal);
    Ok(())
}

#[async_scheme_fn]
pub async fn get_goals_events(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let mut f = String::new();
    writeln!(f, "# GOALS:").unwrap();
    for (i, goal) in ctx.goals.read().await.iter().enumerate() {
        writeln!(f, "{i:3} : {goal}").unwrap();
    }

    writeln!(f, "\n# TASKS:").unwrap();
    for (i, task) in ctx.tasks.read().await.iter().enumerate() {
        writeln!(f, "{i:3} : {task}").unwrap();
    }

    writeln!(f, "\n# EVENTS:").unwrap();
    for (i, task) in ctx.events.read().await.iter().enumerate() {
        writeln!(f, "{i:3} : {task}").unwrap();
    }

    Ok(f)
}

#[async_scheme_fn]
pub async fn remove_goal(env: &LEnv, goal_id: usize) -> Result<(), LRuntimeError> {
    let ctx: &ModPlanning = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let _ = ctx.goals.write().await.remove(goal_id);
    Ok(())
}

#[async_scheme_fn]
pub async fn remove_task(env: &LEnv, task_id: usize) -> Result<(), LRuntimeError> {
    let ctx: &ModPlanning = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let _ = ctx.tasks.write().await.remove(task_id);
    Ok(())
}

#[async_scheme_fn]
pub async fn remove_event(env: &LEnv, event_id: usize) -> Result<(), LRuntimeError> {
    let ctx: &ModPlanning = env.get_context::<ModPlanning>(MOD_PLANNING)?;

    let _ = ctx.events.write().await.remove(event_id);
    Ok(())
}
