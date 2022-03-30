use aries_model::extensions::{AssignmentExt, SavedAssignment};
use aries_model::lang::SAtom;
use aries_planners::encode::{
    encode, populate_with_task_network, populate_with_template_instances,
};
use aries_planners::fmt::{format_hddl_plan, format_partial_plan, format_pddl_plan};
use aries_planners::solver::Strat;
use aries_planners::{ParSolver, Solver};
use aries_planning::chronicles;
use aries_planning::chronicles::analysis::hierarchical_is_non_recursive;
use aries_planning::chronicles::{ChronicleKind, FiniteProblem};
use aries_tnet::theory::{StnConfig, StnTheory, TheoryPropagationLevel};
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use std::sync::Arc;
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

pub struct PlanResult {
    pub ass: Arc<SavedAssignment>,
    pub fp: FiniteProblem,
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
        tables: base_problem.context.tables.clone(),
    };
    if htn_mode {
        populate_with_task_network(&mut pb, &base_problem, depth).unwrap();
    } else {
        populate_with_template_instances(&mut pb, &base_problem, |_| Some(depth)).unwrap();
    }

    let mut solver = init_solver(&pb);
    if solver.propagate_and_backtrack_to_consistent() {
        let str = format_partial_plan(&pb, &solver.model).unwrap();
        println!("{}", str);
    } else {
        panic!("Invalid problem (propagation failed)");
    }
}

pub fn run_solver(problem: &mut chronicles::Problem, htn_mode: bool) -> Option<PlanResult> {
    println!("===== Preprocessing ======");
    aries_planning::chronicles::preprocessing::preprocess(problem);
    println!("==========================");

    let max_depth = u32::MAX;
    let min_depth = if htn_mode && hierarchical_is_non_recursive(problem) {
        max_depth // non recursive htn: bounded size, go directly to max
    } else {
        0
    };

    //propagate_and_print(problem, min_depth, htn_mode);

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
            tables: problem.context.tables.clone(),
        };
        if htn_mode {
            populate_with_task_network(&mut pb, problem, n).unwrap();
        } else {
            populate_with_template_instances(&mut pb, problem, |_| Some(n)).unwrap();
        }
        println!("  [{:.3}s] Populated", start.elapsed().as_secs_f32());
        let start = Instant::now();
        let solver_result = solve(&pb, htn_mode);
        println!("  [{:.3}s] solved", start.elapsed().as_secs_f32());
        if let Some(x) = solver_result {
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
            result = Some(PlanResult { ass: x, fp: pb });
            break;
        } else {
            println!("no solution found");
        }
    }
    result
}

pub fn extract_instantiated_methods(pr: &PlanResult) -> LResult {
    let ass = &pr.ass;
    let problem = &pr.fp;

    let methods: Vec<_> = pr
        .fp
        .chronicles
        .iter()
        .filter(|ch| {
            ass.boolean_value_of(ch.chronicle.presence) == Some(true)
                && ch.chronicle.kind == ChronicleKind::Method
        })
        .collect();

    let fmt1 = |x: &SAtom| -> LValue {
        let sym = ass.sym_domain_of(*x).into_singleton().unwrap();
        problem.model.shape.symbols.symbol(sym).to_string().into()
    };
    let mut lv_methods: Vec<LValue> = vec![];
    for m in methods {
        let name: Vec<LValue> = m.chronicle.name.iter().map(|s| fmt1(s)).collect::<_>();
        lv_methods.push(name.into());
    }

    Ok(lv_methods.into())
}
