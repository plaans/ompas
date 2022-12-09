use crate::exec::refinement::greedy_select;
use crate::exec::task::ModTask;
use crate::exec::ModExec;
use ompas_middleware::logger::LogClient;
use ompas_rae_language::exec::aries::*;
use ompas_rae_language::exec::task::MOD_TASK;
use ompas_rae_planning::aries::binding::solver::run_solver_for_htn;
use ompas_rae_planning::aries::binding::{generate_chronicles, solver};
use ompas_rae_planning::aries::structs::{ConversionCollection, ConversionContext, Problem};
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::domain::OMPASDomain;
use ompas_rae_structs::plan::AbstractTaskInstance;
use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::action_state::{RefinementMetaData, TaskMetaData};
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;

pub struct CtxAries {
    log: LogClient,
    agenda: Agenda,
    domain: Arc<RwLock<OMPASDomain>>,
    cc: Arc<RwLock<ConversionCollection>>,
}

impl CtxAries {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            log: exec.log.clone(),
            agenda: exec.agenda.clone(),
            domain: exec.domain.clone(),
            cc: Arc::new(Default::default()),
        }
    }
}

//Returns the method to do.
pub async fn aries_select(
    state: WorldStateSnapshot,
    tried: &[LValue],
    task: Vec<LValue>,
    env: &LEnv,
    optimize: bool,
) -> lruntimeerror::Result<RefinementMetaData> {
    let mut greedy: RefinementMetaData =
        greedy_select(state.clone(), tried, task.clone(), env).await?;

    let ctx = env.get_context::<CtxAries>(CTX_ARIES)?;
    let log = ctx.log.clone();

    println!("\n\nTask to plan: {}", LValue::from(task.clone()));
    println!("\t*tried: {}", LValue::from(tried));
    println!("\t*greedy: {}", LValue::from(&greedy.applicable_methods));

    let parent_task = env.get_context::<ModTask>(MOD_TASK)?.parent_id;
    match parent_task {
        Some(parent_id) => {
            let parent_stack: TaskMetaData = ctx.agenda.get_task(&parent_id).await?;
            let n = ctx.agenda.get_number_of_subtasks(&parent_id).await - 1;
            println!("{} subtask of {}", n + 1, parent_id);
            println!("Searching for a generated plan...");
            if let Some(plan) = &parent_stack.get_last_refinement().unwrap().plan {
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
                            return Err(lruntimeerror!(
                                SELECT_ARIES,
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
                    greedy.refinement_type = SelectMode::Planning(Planner::Aries(optimize));
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

    let context = ConversionContext {
        domain: ctx.domain.read().await.clone(),
        env: env.clone(),
        state,
    };

    let mut problem: Problem = context.borrow().into();
    //let cc = convert_domain_to_chronicle_hierarchy(context)?;
    //println!("cc: {}", cc);
    problem.cc = ctx.cc.as_ref().read().await.clone();
    problem.goal_tasks.push(LValue::from(task).try_into()?);

    let mut aries_problem = generate_chronicles(&problem)?;
    let instant = Instant::now();
    let result = run_solver_for_htn(&mut aries_problem, optimize);

    log.info(format!(
        "Time to run solver: {:^3} ms (optimize = {})",
        instant.elapsed().as_micros() as f64 / 1000.0,
        optimize
    ))
    .await;
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
        greedy.refinement_type = SelectMode::Planning(Planner::Aries(optimize));
        Ok(greedy)
    } else {
        Ok(greedy)
    }
}
