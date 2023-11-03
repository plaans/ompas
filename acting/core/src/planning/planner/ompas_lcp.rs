use crate::model::acting_domain::OMPASDomain;
use crate::ompas::manager::planning::populate_problem;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::planning::{encode, FinitePlanningProblem};
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::StateManager;
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG, OMPAS_PLANNER_OUTPUT, OMPAS_PLAN_OUTPUT};
use anyhow::Result;
use aries::core::{IntCst, INT_CST_MAX};
use aries::model::extensions::{AssignmentExt, SavedAssignment};
use aries::solver::parallel::signals::InputSignal;
use aries::solver::parallel::Solution;
use aries_planners::encode::EncodedProblem;
use aries_planners::fmt::{format_hddl_plan, format_pddl_plan};
use aries_planners::solver::Strat::ActivityNonTemporalFirst;
use aries_planners::solver::{
    init_solver, propagate_and_print, Metric, SolverResult, Strat, HTN_DEFAULT_STRATEGIES,
    PRINT_INITIAL_PROPAGATION, PRINT_MODEL, PRINT_PLANNER_OUTPUT, PRINT_RAW_MODEL,
};
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{ChronicleOrigin, FiniteProblem, TaskId};
use sompas_structs::lenv::LEnv;
use std::collections::HashSet;
use std::sync::Arc;
use std::time::Instant;

const MIN_DEPTH: u32 = 0;
const MAX_DEPTH: u32 = u32::MAX;
const STRATEGIES: [Strat; 2] = [ActivityNonTemporalFirst, Strat::Causal];

pub type PMetric = Metric;

pub type PlannerInterrupter = tokio::sync::watch::Receiver<bool>;

pub type PlannerInterruptSender = tokio::sync::watch::Sender<bool>;

pub struct OMPASLCPConfig {
    pub state_subscriber_id: usize,
    pub opt: Option<PMetric>,
    pub state_manager: StateManager,
    pub domain: Arc<OMPASDomain>,
    pub env: LEnv,
}

pub fn is_fully_populated(instances: &[ChronicleInstance]) -> bool {
    let origins: HashSet<ChronicleOrigin> = instances.iter().map(|c| c.origin.clone()).collect();

    for (instance_id, c) in instances.iter().enumerate() {
        for (task_id, _) in c.instantiated_chronicle.get_subtasks().iter().enumerate() {
            let origin = ChronicleOrigin::Refinement {
                refined: vec![TaskId {
                    instance_id,
                    task_id,
                }],
                template_id: 0,
            };

            if !origins.contains(&origin) {
                return false;
            }
        }
    }
    true
}

pub async fn run_planner(
    execution_problem: &ExecutionProblem,
    config: &OMPASLCPConfig,
    on_new_sol: impl Fn(&FiniteProblem, Arc<SavedAssignment>) + Clone,
    interrupter: Option<PlannerInterrupter>,
) -> Result<Option<PlanResult>> {
    let OMPASLCPConfig {
        state_subscriber_id,
        opt,
        state_manager,
        domain,
        env,
    } = config;

    let mut best_cost = INT_CST_MAX + 1;

    let start = Instant::now();

    let mut pp = populate_problem(
        FinitePlanningProblem::ExecutionProblem(execution_problem),
        domain,
        env,
        0,
    )
    .await
    .unwrap();

    let min_depth = MIN_DEPTH;
    let max_depth = MAX_DEPTH;

    for depth in min_depth..=max_depth {
        let depth_string = if depth == u32::MAX {
            "∞".to_string()
        } else {
            depth.to_string()
        };
        if PRINT_PLANNER_OUTPUT.get() {
            println!("{depth_string} Solving with depth {depth_string}");
        }
        break;

        pp = populate_problem(FinitePlanningProblem::PlannerProblem(&pp), domain, env, 1)
            .await
            .unwrap();
        let fully_populated = is_fully_populated(&pp.instances);

        if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
            for (origin, chronicle) in pp
                .instances
                .iter()
                .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
            {
                println!("{:?}:\n{}", origin, chronicle)
            }
        }

        let rule = StateRule::Specific(
            pp.domain
                .sf
                .iter()
                .map(|sf| sf.get_label().into())
                .collect(),
        );

        state_manager
            .update_subscriber_rule(state_subscriber_id, rule)
            .await;

        let (mut problem, table) = encode(&pp).await.unwrap();

        if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
            for instance in &problem.chronicles {
                Printer::print_chronicle(&instance.chronicle, &problem.context.model);
            }
        }

        if PRINT_RAW_MODEL.get() {
            Printer::print_problem(&problem);
        }
        if PRINT_PLANNER_OUTPUT.get() {
            println!("===== Preprocessing ======");
        }
        aries_planning::chronicles::preprocessing::preprocess(&mut problem);
        if PRINT_PLANNER_OUTPUT.get() {
            println!("==========================");
        }

        if PRINT_MODEL.get() {
            Printer::print_problem(&problem);
        }

        let mut pb = FiniteProblem {
            model: problem.context.model.clone(),
            origin: problem.context.origin(),
            horizon: problem.context.horizon(),
            chronicles: problem.chronicles.clone(),
        };

        aries_planners::encode::populate_with_task_network(&mut pb, &problem, depth)?;
        let pb = Arc::new(pb);

        let on_new_valid_assignment = {
            let pb = pb.clone();
            let on_new_sol = on_new_sol.clone();
            move |ass: Arc<SavedAssignment>| on_new_sol(&pb, ass)
        };
        if PRINT_PLANNER_OUTPUT.get() {
            println!("  [{:.3}s] Populated", start.elapsed().as_secs_f32());
        }
        let result = solve_finite_problem(
            pb.clone(),
            &STRATEGIES,
            *opt,
            on_new_valid_assignment,
            best_cost - 1,
            interrupter.clone(),
        ).await;
        if PRINT_PLANNER_OUTPUT.get() {
            println!("  [{:.3}s] Solved", start.elapsed().as_secs_f32());
        }

        let result = result.map(|assignment| (pb, assignment));
        match result {
            SolverResult::Unsat => {
                //println!("unsat")
            } // continue (increase depth)
            SolverResult::Sol((_, (_, cost)))
                if opt.is_some() && depth < max_depth && !fully_populated =>
            {
                //println!("get sol");
                let cost = cost.expect("Not cost provided in optimization problem");
                assert!(cost < best_cost);
                best_cost = cost; // continue with new cost bound
            }
            other => {
                return Ok(other.map(|(pb, (ass, _))| (pb, ass))).map(|r| match r {
                    SolverResult::Sol((fp, ass)) => {
                        if OMPAS_PLANNER_OUTPUT.get() {
                            println!("  Solution found");
                            println!(
                                "\n**** Decomposition ****\n\n\
                    {}\n\n\
                    **** Plan ****\n\n\
                    {}",
                                format_hddl_plan(&fp, &ass).unwrap(),
                                format_pddl_plan(&fp, &ass).unwrap()
                            );
                        }

                        Some(PlanResult {
                            ass,
                            fp,
                            pp: Arc::new(pp),
                            table: Arc::new(table),
                        })
                    }
                    SolverResult::Unsat => {
                        if OMPAS_PLANNER_OUTPUT.get() {
                            println!("No solution");
                        }
                        None
                    }
                    SolverResult::Timeout(_) => {
                        if OMPAS_PLANNER_OUTPUT.get() {
                            println!("Timeout");
                        }
                        None
                    }
                    SolverResult::Interrupt(_) => {
                        if OMPAS_PLAN_OUTPUT.get() {
                            println!("Interrupt")
                        }
                        None
                    }
                })
            }
        }
    }

    Ok(None)
}

/// Instantiates a solver for the given subproblem and attempts to solve it.
///
/// If more than one strategy is given, each strategy will have its own solver run on a dedicated thread.
/// If no strategy is given, then a default set of strategies will be automatically selected.
///
/// If a valid solution of the subproblem is found, the solver will return a satisfying assignment.
#[allow(clippy::too_many_arguments)]
async fn solve_finite_problem(
    pb: Arc<FiniteProblem>,
    strategies: &[Strat],
    metric: Option<Metric>,
    on_new_solution: impl Fn(Arc<SavedAssignment>),
    cost_upper_bound: IntCst,
    interrupter: Option<PlannerInterrupter>,
) -> SolverResult<(Solution, Option<IntCst>)> {
    if PRINT_INITIAL_PROPAGATION.get() {
        propagate_and_print(&pb);
    }
    let Ok(EncodedProblem {
        mut model,
        objective: metric,
        encoding,
    }) = aries_planners::encode::encode(&pb, metric)
    else {
        return SolverResult::Unsat;
    };
    if let Some(metric) = metric {
        model.enforce(metric.le_lit(cost_upper_bound), []);
    }
    let solver = init_solver(model);
    let encoding = Arc::new(encoding);

    // select the set of strategies, based on user-input or hard-coded defaults.
    let strats: &[Strat] = if !strategies.is_empty() {
        strategies
    } else {
        &HTN_DEFAULT_STRATEGIES
    };
    let mut solver = aries::solver::parallel::ParSolver::new(solver, strats.len(), |id, s| {
        strats[id].adapt_solver(s, pb.clone(), encoding.clone())
    });

    let input_stream = solver.input_stream();
    tokio::spawn(async move {
        if let Some(mut interrupter) = interrupter {
            if interrupter.wait_for(|b| *b == true).await.is_ok() {
                println!("interrupt received");
                let _ = input_stream.sender.send(InputSignal::Interrupt);
            }
        }
    });

    let result = if let Some(metric) = metric {
        solver.minimize_with(metric, on_new_solution, None)
    } else {
        solver.solve(None)
    };

    // tag result with cost
    let result = result.map(|s| {
        let cost = metric.map(|metric| s.domain_of(metric).0);
        (s, cost)
    });

    if let SolverResult::Sol(_) = result {
        if PRINT_PLANNER_OUTPUT.get() {
            solver.print_stats()
        }
    }
    result
}
