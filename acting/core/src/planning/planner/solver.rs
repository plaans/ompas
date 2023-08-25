use crate::planning::planner::result::PlanResult;
use crate::OMPAS_PLANNER_OUTPUT;
use anyhow::Result;
use aries_planners::fmt::{format_hddl_plan, format_pddl_plan};
use aries_planners::solver::Strat::ActivityNonTemporalFirst;
use aries_planners::solver::{solve, Metric, SolverResult, Strat};
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use std::time::Instant;

const MIN_DEPTH: u32 = 0;
const MAX_DEPTH: u32 = u32::MAX;
const STRATEGIES: [Strat; 4] = [
    ActivityNonTemporalFirst,
    ActivityNonTemporalFirst,
    //Strat::Forward,
    Strat::Causal,
    Strat::Causal,
    //Strat::Activity,
];

pub type PMetric = Metric;

pub fn run_planner(
    problem: chronicles::Problem,
    optimize: Option<PMetric>,
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
    );
    if OMPAS_PLANNER_OUTPUT.get() {
        println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());
    }

    result.map(|r| {
        if let SolverResult::Sol((fp, ass)) = r {
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
        } else if let SolverResult::Unsat = r {
            if OMPAS_PLANNER_OUTPUT.get() {
                println!("No solution");
            }
            None
        } else {
            None
        }
    })
}

/*
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
) -> Result<SolverResult<(Arc<FiniteProblem>, Arc<Domains>)>> {
    println!("===== Preprocessing ======");
    aries_planning::chronicles::preprocessing::preprocess(&mut base_problem);
    println!("==========================");

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
        println!("{depth_string} Solving with {depth_string} actions");
        if htn_mode {
            populate_with_task_network(&mut pb, &base_problem, depth)?;
        } else {
            populate_with_template_instances(&mut pb, &base_problem, |_| Some(depth))?;
        }
        let pb = Arc::new(pb);

        let on_new_valid_assignment = {
            let pb = pb.clone();
            let on_new_sol = on_new_sol.clone();
            move |ass: Arc<SavedAssignment>| on_new_sol(&pb, ass)
        };
        println!("  [{:.3}s] Populated", start.elapsed().as_secs_f32());
        let result = solve_finite_problem(
            &pb,
            strategies,
            metric,
            htn_mode,
            on_new_valid_assignment,
            deadline,
        );
        println!("  [{:.3}s] Solved", start.elapsed().as_secs_f32());

        let result = result.map(|assignment| (pb, assignment));
        match result {
            SolverResult::Unsat => {} // continue (increase depth)
            other => return Ok(other),
        }
    }
    Ok(SolverResult::Unsat)
}

fn solve_finite_problem(
    pb: &FiniteProblem,
    strategies: &[Strat],
    metric: Option<Metric>,
    htn_mode: bool,
    on_new_solution: impl Fn(Arc<SavedAssignment>),
    deadline: Option<Instant>,
) -> SolverResult<Solution> {
    if PRINT_INITIAL_PROPAGATION.get() {
        propagate_and_print(pb);
    }
    let (solver, metric) = init_solver(pb, metric);

    // select the set of strategies, based on user-input or hard-coded defaults.
    let strats: &[Strat] = if !strategies.is_empty() {
        strategies
    } else if htn_mode {
        &HTN_DEFAULT_STRATEGIES
    } else {
        &GEN_DEFAULT_STRATEGIES
    };
    let mut solver = aries::solver::parallel::ParSolver::new(solver, strats.len(), |id, s| {
        strats[id].adapt_solver(s, pb)
    });

    let result = if let Some(metric) = metric {
        solver.minimize_with(metric, on_new_solution, deadline)
    } else {
        solver.solve(deadline)
    };

    if let SolverResult::Sol(_) = result {
        solver.print_stats()
    }
    result
}*/
