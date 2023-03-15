use crate::aries::result::PlanResult;
use anyhow::Result;
use aries::reasoners::cp::Cp;
use aries::reasoners::stn::theory::{StnConfig, StnTheory, TheoryPropagationLevel};
use aries::solver::parallel::ParSolver;
use aries::solver::parallel::Solution;
use aries_planners::encode::{
    encode, populate_with_task_network, populate_with_template_instances,
};
use aries_planners::fmt::{format_hddl_plan, format_partial_plan, format_pddl_plan};
use aries_planners::solver::Strat::ActivityNonTemporalFirst;
use aries_planners::solver::{init_solver, solve, Metric, SolverResult, Strat};
use aries_planners::Solver;
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use aries_planning::chronicles::{FiniteProblem, VarLabel};
use std::os::unix::raw::uid_t;
use std::time::Instant;

/// Default set of strategies for HTN problems
const HTN_DEFAULT_STRATEGIES: [Strat; 2] = [Strat::Activity, Strat::Forward];
/// Default set of strategies for generative (flat) problems.

const STRATEGIES: [Strat; 2] = [ActivityNonTemporalFirst, Strat::Forward];
const MIN_DEPTH: u32 = 0;
const MAX_DEPTH: u32 = u32::MAX;

pub fn run_solver(
    problem: chronicles::Problem,
    optimize: Option<Metric>,
) -> Result<Option<PlanResult>> {
    let start = Instant::now();

    let result = solve(
        problem,
        MIN_DEPTH,
        MAX_DEPTH,
        &STRATEGIES,
        optimize,
        true,
        |_, _| {},
        None,
    );

    println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());

    result.map(|r| {
        if let SolverResult::Sol((fp, ass)) = r {
            println!("  Solution found");
            println!(
                "\n**** Decomposition ****\n\n\
                    {}\n\n\
                    **** Plan ****\n\n\
                    {}",
                format_hddl_plan(&fp, &ass).unwrap(),
                format_pddl_plan(&fp, &ass).unwrap()
            );

            Some(PlanResult { ass, fp })
        } else {
            None
        }
    })
}
