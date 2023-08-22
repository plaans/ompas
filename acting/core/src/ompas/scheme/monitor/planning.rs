use crate::model::acting_domain::model::{Event, Goal, NewTask};
use crate::model::acting_domain::OMPASDomain;
use crate::model::add_domain_symbols;
use crate::model::sym_domain::cst;
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::acting::planning::{
    encode, extract_choices, populate_problem, ActingVarRefTable,
};
use crate::ompas::manager::state::world_state::WorldStateSnapshot;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::ompas::scheme::monitor::ModMonitor;
use crate::planning::planner::encoding::PlannerProblem;
use crate::planning::planner::problem::new_problem_chronicle_instance;
use crate::planning::planner::result::PlanResult;
use crate::planning::planner::solver::{run_planner, PMetric};
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG_ON, OMPAS_PLAN_OUTPUT_ON};
use aries_planning::chronicles;
use aries_planning::chronicles::printer::Printer;
use ompas_language::monitor::control::MOD_CONTROL;
use ompas_language::monitor::model::MOD_MODEL;
use ompas_language::monitor::planning::*;
use sompas_macros::async_scheme_fn;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};

#[derive(Clone)]
pub struct ModPlanning {
    events: Arc<RwLock<Vec<Event>>>,
    goals: Arc<RwLock<Vec<Goal>>>,
    tasks: Arc<RwLock<Vec<NewTask>>>,
    _st: RefSymTable,
    _domain: Arc<RwLock<OMPASDomain>>,
}

impl ModPlanning {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            events: Arc::new(Default::default()),
            goals: Arc::new(Default::default()),
            tasks: Arc::new(Default::default()),
            _st: monitor.acting_manager.st.clone(),
            _domain: monitor.acting_manager.domain.clone(),
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

async fn _plan_in_ompas(env: &LEnv, args: &[LValue], opt: bool) -> LResult {
    let task: LValue = args.into();
    println!("task to plan: {}", task);
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    //let mut context: ConversionContext = ctx.get_conversion_context().await;
    let mut env: LEnv = ctx.get_plan_env().await;
    let state = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state));

    acting_manager
        .start_continuous_planning(env, if opt { Some(PMetric::Makespan) } else { None })
        .await;

    let debug = LValue::from(args).to_string();
    let args = args.iter().map(|lv| lv.as_cst().unwrap()).collect();

    let _pr = acting_manager.new_high_level_task(debug, args).await;
    let mut recv: broadcast::Receiver<bool> =
        acting_manager.subscribe_on_plan_update().await.unwrap();
    recv.recv()
        .await
        .expect("Error while waiting on plan update.");
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn plan_in_ompas(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_in_ompas(env, args, false).await
}

#[async_scheme_fn]
pub async fn plan_opt_in_ompas(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_in_ompas(env, args, true).await
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
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let mut env: LEnv = ctx.get_plan_env().await;
    let state: WorldStateSnapshot = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state));
    let opt = if opt { Some(PMetric::Makespan) } else { None };
    let st = acting_manager.st.clone();
    let domain: OMPASDomain = acting_manager.domain.read().await.clone();
    add_domain_symbols(&st, &domain);

    let mut state = acting_manager.state.get_snapshot().await;
    let resource_state = acting_manager.resource_manager.get_snapshot().await;
    state.absorb(resource_state);
    let ep: ExecutionProblem = ExecutionProblem {
        state,
        chronicles: vec![new_problem_chronicle_instance(&st, tasks, goals, events)],
    };

    let pp: PlannerProblem = populate_problem(&domain, &env, &st, ep).await.unwrap();
    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
        for (origin, chronicle) in pp
            .instances
            .iter()
            .map(|i| (i.origin.clone(), i.am.chronicle.as_ref().unwrap()))
        {
            println!("{:?}:{}", origin, chronicle)
        }
    }

    let (aries_problem, table): (chronicles::Problem, ActingVarRefTable) =
        encode(&st, &pp).await.unwrap();
    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
        for instance in &aries_problem.chronicles {
            Printer::print_chronicle(&instance.chronicle, &aries_problem.context.model);
        }
    }

    let result = run_planner(aries_problem, opt);
    //println!("{}", format_partial_plan(&pb, &x)?);

    if let Ok(Some(pr)) = result {
        //result::print_chronicles(&pr);
        let PlanResult { ass, fp } = pr;

        let choices = extract_choices(&table, &ass, &fp.model, &pp);
        //let new_ams = extract_new_acting_models(&table, &ass, &fp.model, pp);

        if OMPAS_PLAN_OUTPUT_ON.get() {
            println!("Plan found");
            for choice in &choices {
                println!("{}:{}", choice.process_ref, choice.choice_inner)
            }
        }
    } else {
        println!("No solution found by planner")
    };

    Ok(())
}

fn read_slice(slice: &[LValue]) -> Result<Vec<cst::Cst>, LRuntimeError> {
    let mut args = vec![];
    for arg in slice {
        args.push(read_value(arg).map_err(|e| e.chain("read_slice"))?)
    }
    Ok(args)
}

fn read_value(lv: &LValue) -> Result<cst::Cst, LRuntimeError> {
    lv.as_cst().ok_or(LRuntimeError::wrong_type(
        "read_value",
        lv,
        KindLValue::Atom,
    ))
}

fn read_moment(moment: &LValue) -> Result<Interval, LRuntimeError> {
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

    for arg in &task_args[1..args.len()] {
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
pub async fn new_event(
    env: &LEnv,
    sv: LValue,
    value: LValue,
    moment: LValue,
) -> Result<(), LRuntimeError> {
    let sv: Vec<cst::Cst> = if let LValue::List(list) = sv {
        read_slice(list.as_slice()).map_err(|e| e.chain(NEW_EVENT))?
    } else {
        vec![read_value(&sv).map_err(|e| e.chain(NEW_EVENT))?]
    };

    let value: Cst = read_value(&value).map_err(|e| e.chain(NEW_EVENT))?;

    let interval = read_moment(&moment).map_err(|e| e.chain(NEW_EVENT))?;

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
