use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::refinement::greedy_select;
use crate::ompas::scheme::exec::ModExec;
use crate::planning::planner::problem::PlanningDomain;
use ompas_middleware::logger::LogClient;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct CtxAries {
    _log: LogClient,
    _acting_manager: ActingManager,
    _domain: DomainManager,
    _pd: Arc<RwLock<Option<PlanningDomain>>>,
}

impl CtxAries {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            _log: exec.log.clone(),
            _acting_manager: exec.acting_manager.clone(),
            _domain: exec.domain.clone(),
            _pd: Arc::new(Default::default()),
        }
    }
}

//Returns the method to do.
pub async fn aries_select(
    _candidates: &[LValue],
    _state: &WorldStateSnapshot,
    _env: &LEnv,
    _optimize: bool,
) -> lruntimeerror::Result<LValue> {
    greedy_select(_candidates, _state, _env)
    /*let ctx = env.get_context::<CtxAries>(CTX_ARIES)?;
    let log = ctx.log.clone();

    println!("\n\nTask to plan: {}", LValue::from(task.clone()));
    println!("\t*tried: {}", LValue::from(tried));
    println!("\t*greedy: {}", LValue::from(&greedy.applicable_methods));

    let parent_task = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .get_task_id()
        .await;
    match parent_task {
        Some(parent_id) => {
            let parent_stack: TaskMetaData = ctx.supervisor.get_task(&parent_id).await?;
            let n = ctx.supervisor.get_number_of_subtasks(&parent_id).await - 1;
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
                    greedy.interval.set_end(ctx.supervisor.get_instant());
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

    let problem = PlanningProblem {
        domain: ctx.pd.read().await.clone().unwrap(),
        instance: PlanningInstance {
            state,
            tasks: vec![LValue::from(task).try_into()?],
            instances: vec![],
        },
        st: Default::default(),
    };

    let mut bindings = BindingAriesAtoms::default();

    let mut aries_problem = generate_templates(&problem, &mut bindings)?;
    let instant = Instant::now();
    let result = run_solver_for_htn(&mut aries_problem, optimize);

    log.info(format!(
        "Time to run solver: {:^3} ms (optimize = {})",
        instant.elapsed().as_micros() as f64 / 1000.0,
        optimize
    ))
    .await;
    // println!("{}", format_partial_plan(&pb, &x)?);

    let mut greedy: RefinementTrace = greedy;

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
        greedy.interval.set_end(ctx.supervisor.get_instant());
        greedy.refinement_type = SelectMode::Planning(Planner::Aries(optimize));
        Ok(greedy)
    } else {
        Ok(greedy)
    }*/
}
