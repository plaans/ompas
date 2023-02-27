use crate::aries::result::PlanResult;
use aries_cp::Cp;
use aries_planners::encode::{
    encode, populate_with_task_network, populate_with_template_instances,
};
use aries_planners::fmt::{format_hddl_plan, format_partial_plan, format_pddl_plan};
use aries_planners::solver::{SolverResult, Strat};
use aries_planners::Solver;
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use aries_planning::chronicles::FiniteProblem;
use aries_solver::parallel_solver::Solution;
use aries_stn::theory::{StnConfig, StnTheory, TheoryPropagationLevel};
use std::time::Instant;

fn init_solver(pb: &FiniteProblem) -> Box<Solver> {
    let model = encode(pb, None).unwrap();
    let stn_config = StnConfig {
        theory_propagation: TheoryPropagationLevel::Full,
        ..Default::default()
    };

    let mut solver = Box::new(aries_solver::solver::Solver::new(model.0));
    solver.add_theory(|tok| StnTheory::new(tok, stn_config));
    solver.add_theory(Cp::new);
    solver
}

/// Default set of strategies for HTN problems
const HTN_DEFAULT_STRATEGIES: [Strat; 2] = [Strat::Activity, Strat::Forward];
/// Default set of strategies for generative (flat) problems.
//const GEN_DEFAULT_STRATEGIES: [Strat; 1] = [Strat::Activity];

fn solve_htn(pb: &FiniteProblem, optimize: bool) -> Option<Solution> {
    let solver = init_solver(pb);
    let strats: &[Strat] = &HTN_DEFAULT_STRATEGIES;

    let mut solver =
        aries_solver::parallel_solver::ParSolver::new(solver, strats.len(), |id, s| {
            strats[id].adapt_solver(s, pb)
        });

    let found_plan: Option<Solution> = if optimize {
        let res = solver.minimize(pb.horizon.num, None);
        match res {
            SolverResult::Sol(sol) => Some(sol),
            _ => None,
        }
    } else {
        if let SolverResult::Sol(sol) = solver.solve(None) {
            Some(sol)
        } else {
            None
        }
    };

    if let Some(solution) = found_plan {
        solver.print_stats();
        Some(solution)
    } else {
        None
    }
}

/// This function mimics the instantiation of the subproblem of the given `depth`, run the propagation
/// and exits immediately.
///
/// Note that is meant to facilitate debugging of the planner during development.
#[allow(dead_code)]
fn propagate_and_print(base_problem: &mut chronicles::Problem, depth: u32, htn_mode: bool) {
    println!("===== Preprocessing ======");
    aries_planning::chronicles::preprocessing::preprocess(base_problem);
    println!("==========================");

    let mut pb = FiniteProblem {
        model: base_problem.context.model.clone(),
        origin: base_problem.context.origin(),
        horizon: base_problem.context.horizon(),
        chronicles: base_problem.chronicles.clone(),
    };
    if htn_mode {
        populate_with_task_network(&mut pb, base_problem, depth).unwrap();
    } else {
        populate_with_template_instances(&mut pb, base_problem, |_| Some(depth)).unwrap();
    }

    let mut solver = init_solver(&pb);
    if solver.propagate_and_backtrack_to_consistent() {
        let str = format_partial_plan(&pb, &solver.model).unwrap();
        println!("{}", str);
    } else {
        panic!("Invalid problem (propagation failed)");
    }
}

pub fn run_solver_for_htn(problem: &mut chronicles::Problem, optimize: bool) -> Option<PlanResult> {
    println!("===== Preprocessing ======");
    aries_planning::chronicles::preprocessing::preprocess(problem);
    println!("==========================");

    let max_depth = u32::MAX;
    let min_depth = if hierarchical_is_non_recursive(problem) {
        max_depth // non recursive htn: bounded size, go directly to max
    } else {
        0
    };

    //propagate_and_print(problem, min_depth, true);

    let mut result = None;

    for n in min_depth..=max_depth {
        let depth_string = if n == u32::MAX {
            "∞".to_string()
        } else {
            n.to_string()
        };
        println!("{} Solving with {} actions", depth_string, depth_string);
        let start = Instant::now();
        let mut pb = FiniteProblem {
            model: problem.context.model.clone(),
            origin: problem.context.origin(),
            horizon: problem.context.horizon(),
            chronicles: problem.chronicles.clone(),
        };
        populate_with_task_network(&mut pb, problem, n).unwrap();

        println!("  [{:.3}s] Populated", start.elapsed().as_secs_f32());
        let start = Instant::now();
        let solver_result = solve_htn(&pb, optimize);
        println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());
        if let Some(x) = solver_result {
            println!("  Solution found");
            println!(
                "\n**** Decomposition ****\n\n\
                    {}\n\n\
                    **** Plan ****\n\n\
                    {}",
                format_hddl_plan(&pb, &x).unwrap(),
                format_pddl_plan(&pb, &x).unwrap()
            );

            result = Some(PlanResult { ass: x, fp: pb });
            break;
        } else {
            println!("no solution found");
        }
    }
    result
}
/*
Plan example formatted by format_hddl_plan

3 (drive t1 l2)
6 (drive t1 l4)
root 1
1 (t_move t1 l4) -> m_recursive 3 4
4 (t_move t1 l4) -> m_recursive 6 7
7 (t_move t1 l4) -> m_already_there
*/
