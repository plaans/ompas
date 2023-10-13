use crate::planning::planner::result::PlanResult;
use crate::{OMPAS_PLANNER_OUTPUT, OMPAS_PLAN_OUTPUT};
use anyhow::Result;
use aries::core::state::Domains;
use aries::core::{IntCst, INT_CST_MAX};
use aries::model::extensions::{AssignmentExt, SavedAssignment};
use aries::solver::parallel::signals::InputSignal;
use aries::solver::parallel::Solution;
use aries_planners::encode::EncodedProblem;
use aries_planners::fmt::{format_hddl_plan, format_pddl_plan};
use aries_planners::solver::Strat::ActivityNonTemporalFirst;
use aries_planners::solver::{
    init_solver, propagate_and_print, Metric, SolverResult, Strat, GEN_DEFAULT_STRATEGIES,
    HTN_DEFAULT_STRATEGIES, PRINT_INITIAL_PROPAGATION, PRINT_MODEL, PRINT_PLANNER_OUTPUT,
    PRINT_RAW_MODEL,
};
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{FiniteProblem, Problem};
use std::sync::Arc;
use std::time::Instant;

const MIN_DEPTH: u32 = 0;
const MAX_DEPTH: u32 = u32::MAX;
const STRATEGIES: [Strat; 2] = [ActivityNonTemporalFirst, Strat::Causal];

pub type PMetric = Metric;

pub type PlannerInterrupter = tokio::sync::watch::Receiver<bool>;

pub type PlannerInterruptSender = tokio::sync::watch::Sender<bool>;

pub fn run_planner(
    problem: chronicles::Problem,
    optimize: Option<PMetric>,
    interrupter: Option<PlannerInterrupter>,
) -> Result<Option<PlanResult>> {
    let start = Instant::now();
    let max_depth = MAX_DEPTH;

    let min_depth = if hierarchical_is_non_recursive(&problem) {
        max_depth // non recursive htn: bounded size, go directly to max
    } else {
        MIN_DEPTH
    };

    let result = solve(
        problem,
        min_depth,
        max_depth,
        &STRATEGIES,
        optimize,
        true,
        |_, _| {},
        None,
        interrupter,
    );
    if OMPAS_PLANNER_OUTPUT.get() {
        println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());
    }

    result.map(|r| match r {
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

            Some(PlanResult { ass, fp })
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

/// Search for plan based on the `base_problem`.
///
/// The solver will look for plan by generating subproblem of increasing `depth`
/// (for `depth` in `{min_depth, max_depth]`) where `depth` defines the number of allowed actions
/// in the subproblem.
///
/// The `depth` parameter is increased until a plan is found or foes over `max_depth`.
///
/// When a plan is found, the solver returns the corresponding subproblem and the instantiation of
/// its variables.
#[allow(clippy::too_many_arguments)]
pub fn solve(
    mut base_problem: Problem,
    min_depth: u32,
    max_depth: u32,
    strategies: &[Strat],
    metric: Option<Metric>,
    htn_mode: bool,
    on_new_sol: impl Fn(&FiniteProblem, Arc<SavedAssignment>) + Clone,
    deadline: Option<Instant>,
    interrupter: Option<PlannerInterrupter>,
) -> Result<SolverResult<(Arc<FiniteProblem>, Arc<Domains>)>> {
    if PRINT_RAW_MODEL.get() {
        Printer::print_problem(&base_problem);
    }
    if PRINT_PLANNER_OUTPUT.get() {
        println!("===== Preprocessing ======");
    }
    aries_planning::chronicles::preprocessing::preprocess(&mut base_problem);
    if PRINT_PLANNER_OUTPUT.get() {
        println!("==========================");
    }
    if PRINT_MODEL.get() {
        Printer::print_problem(&base_problem);
    }

    let mut best_cost = INT_CST_MAX + 1;

    let start = Instant::now();
    for depth in min_depth..=max_depth {
        let mut pb = FiniteProblem {
            model: base_problem.context.model.clone(),
            origin: base_problem.context.origin(),
            horizon: base_problem.context.horizon(),
            chronicles: base_problem.chronicles.clone(),
        };
        let depth_string = if depth == u32::MAX {
            "∞".to_string()
        } else {
            depth.to_string()
        };
        if PRINT_PLANNER_OUTPUT.get() {
            println!("{depth_string} Solving with depth {depth_string}");
        }
        if htn_mode {
            aries_planners::encode::populate_with_task_network(&mut pb, &base_problem, depth)?;
        } else {
            aries_planners::encode::populate_with_template_instances(
                &mut pb,
                &base_problem,
                |_| Some(depth),
            )?;
        }
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
            strategies,
            metric,
            htn_mode,
            on_new_valid_assignment,
            deadline,
            best_cost - 1,
            interrupter.clone(),
        );
        if PRINT_PLANNER_OUTPUT.get() {
            println!("  [{:.3}s] Solved", start.elapsed().as_secs_f32());
        }

        let result = result.map(|assignment| (pb, assignment));
        match result {
            SolverResult::Unsat => {} // continue (increase depth)
            SolverResult::Sol((_, (_, cost))) if metric.is_some() && depth < max_depth => {
                let cost = cost.expect("Not cost provided in optimization problem");
                assert!(cost < best_cost);
                best_cost = cost; // continue with new cost bound
            }
            other => return Ok(other.map(|(pb, (ass, _))| (pb, ass))),
        }
    }
    Ok(SolverResult::Unsat)
}

/// Instantiates a solver for the given subproblem and attempts to solve it.
///
/// If more than one strategy is given, each strategy will have its own solver run on a dedicated thread.
/// If no strategy is given, then a default set of strategies will be automatically selected.
///
/// If a valid solution of the subproblem is found, the solver will return a satisfying assignment.
#[allow(clippy::too_many_arguments)]
fn solve_finite_problem(
    pb: Arc<FiniteProblem>,
    strategies: &[Strat],
    metric: Option<Metric>,
    htn_mode: bool,
    on_new_solution: impl Fn(Arc<SavedAssignment>),
    deadline: Option<Instant>,
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
    } else if htn_mode {
        &HTN_DEFAULT_STRATEGIES
    } else {
        &GEN_DEFAULT_STRATEGIES
    };
    let mut solver = aries::solver::parallel::ParSolver::new(solver, strats.len(), |id, s| {
        strats[id].adapt_solver(s, pb.clone(), encoding.clone())
    });

    let input_stream = solver.input_stream();
    tokio::spawn(async move {
        if let Some(mut interrupter) = interrupter {
            if interrupter.wait_for(|b| *b).await.is_ok() {
                let _ = input_stream.sender.send(InputSignal::Interrupt);
            }
        }
    });

    let result = if let Some(metric) = metric {
        solver.minimize_with(metric, on_new_solution, deadline)
    } else {
        solver.solve(deadline)
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
