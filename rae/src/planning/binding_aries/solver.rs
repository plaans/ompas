use aries_model::extensions::SavedAssignment;
use aries_planners::encode::{
    encode, populate_with_task_network, populate_with_template_instances,
};
use aries_planners::fmt::{format_hddl_plan, format_pddl_plan};
use aries_planners::solver::Strat;
use aries_planners::{ParSolver, Solver};
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use aries_planning::chronicles::FiniteProblem;
use aries_tnet::theory::{StnConfig, StnTheory, TheoryPropagationLevel};
use std::time::Instant;

fn init_solver(pb: &FiniteProblem) -> Box<Solver> {
    let model = encode(pb).unwrap();
    let stn_config = StnConfig {
        theory_propagation: TheoryPropagationLevel::Full,
        ..Default::default()
    };

    let mut solver = Box::new(aries_solver::solver::Solver::new(model));
    solver.add_theory(|tok| StnTheory::new(tok, stn_config));
    solver
}

/// Default set of strategies for HTN problems
const HTN_DEFAULT_STRATEGIES: [Strat; 2] = [Strat::Activity, Strat::Forward];
/// Default set of strategies for generative (flat) problems.
const GEN_DEFAULT_STRATEGIES: [Strat; 1] = [Strat::Activity];

fn solve(pb: &FiniteProblem, htn_mode: bool) -> Option<std::sync::Arc<SavedAssignment>> {
    let solver = init_solver(pb);
    let strats: &[Strat] = if htn_mode {
        &HTN_DEFAULT_STRATEGIES
    } else {
        &GEN_DEFAULT_STRATEGIES
    };
    let mut solver = if htn_mode {
        aries_solver::parallel_solver::ParSolver::new(solver, strats.len(), |id, s| {
            strats[id].adapt_solver(s, pb)
        })
    } else {
        ParSolver::new(solver, 1, |_, _| {})
    };

    let found_plan = solver.solve().unwrap();

    if let Some(solution) = found_plan {
        solver.print_stats();
        Some(solution)
    } else {
        None
    }
}

pub fn run_solver(problem: &mut chronicles::Problem, htn_mode: bool) {
    println!("===== Preprocessing ======");
    aries_planning::chronicles::preprocessing::preprocess(problem);
    println!("==========================");

    let max_depth = u32::MAX;
    let min_depth = if htn_mode && hierarchical_is_non_recursive(problem) {
        max_depth // non recursive htn: bounded size, go directly to max
    } else {
        0
    };

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
            tables: problem.context.tables.clone(),
        };
        if htn_mode {
            populate_with_task_network(&mut pb, problem, n).unwrap();
        } else {
            populate_with_template_instances(&mut pb, problem, |_| Some(n)).unwrap();
        }
        println!("  [{:.3}s] Populated", start.elapsed().as_secs_f32());
        let start = Instant::now();
        let result = solve(&pb, htn_mode);
        println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());
        if let Some(x) = result {
            // println!("{}", format_partial_plan(&pb, &x)?);
            println!("  Solution found");
            let plan = if htn_mode {
                format!(
                    "\n**** Decomposition ****\n\n\
                    {}\n\n\
                    **** Plan ****\n\n\
                    {}",
                    format_hddl_plan(&pb, &x).unwrap(),
                    format_pddl_plan(&pb, &x).unwrap()
                )
            } else {
                format_pddl_plan(&pb, &x).unwrap()
            };
            println!("{}", plan);
            //let mut file = File::create(output_file).unwrap();
            //file.write_all(plan.as_bytes()).unwrap();
            break;
        } else {
            println!("no solution found");
        }
    }
}
