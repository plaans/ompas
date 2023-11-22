use crate::model::acting_domain::OMPASDomain;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::planning::{encode, FinitePlanningProblem};
use crate::ompas::manager::planning::{populate_problem, DebugDate};
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::StateManager;
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::{ChronicleDebug, OMPAS_CHRONICLE_DEBUG, OMPAS_PLANNER_OUTPUT, OMPAS_PLAN_OUTPUT};
use anyhow::Result;
use aries::core::{IntCst, INT_CST_MAX};
use aries::model::extensions::AssignmentExt;
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
use tokio::runtime::Handle;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;

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
    pub debug_date: DebugDate,
}

fn is_fully_populated(instances: &[ChronicleInstance]) -> bool {
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
    interrupter: Option<PlannerInterrupter>,
    intermediate_sender: Option<UnboundedSender<Result<SolverResult<PlanResult>>>>,
) -> Result<SolverResult<PlanResult>> {
    let OMPASLCPConfig {
        state_subscriber_id,
        opt,
        state_manager,
        domain,
        env,
        debug_date,
    } = config;

    let handle = Handle::current();
    let mut best_cost = INT_CST_MAX + 1;

    let pp = populate_problem(
        FinitePlanningProblem::ExecutionProblem(execution_problem),
        domain,
        env,
        0,
    )
    .await
    .unwrap();
    let mut pp = Arc::new(pp);

    let min_depth = MIN_DEPTH;
    let max_depth = MAX_DEPTH;

    for depth in min_depth..=max_depth {
        if let Some(interrupter) = &interrupter {
            if *interrupter.borrow() {
                return Ok(SolverResult::Interrupt(None));
            }
        }
        let depth_string = if depth == u32::MAX {
            "∞".to_string()
        } else {
            depth.to_string()
        };
        debug_date.print_msg(format!("{depth_string} Solving with depth {depth_string}"));

        let new_pp = populate_problem(FinitePlanningProblem::PlannerProblem(&pp), domain, env, 1)
            .await
            .unwrap();
        if let Some(interrupter) = &interrupter {
            if *interrupter.borrow() {
                return Ok(SolverResult::Interrupt(None));
            }
        }
        debug_date.print_msg("OMPAS Chronicles populated");
        let fully_populated = is_fully_populated(&new_pp.instances);

        if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
            for (origin, chronicle) in new_pp
                .instances
                .iter()
                .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
            {
                println!("{:?}:\n{}", origin, chronicle)
            }
        }

        let rule = StateRule::Specific(
            new_pp
                .domain
                .sf
                .iter()
                .map(|sf| sf.get_label().into())
                .collect(),
        );

        state_manager
            .update_subscriber_rule(state_subscriber_id, rule)
            .await;

        let debug_date = debug_date.clone();
        let interrupter_2 = interrupter.clone();
        let r: Result<_> = handle
            .spawn_blocking(move || {
                let (mut problem, table) = encode(&new_pp).unwrap();
                debug_date.print_msg("Aries chronicles generated");
                if let Some(interrupter) = &interrupter_2 {
                    if *interrupter.borrow() {
                        return Ok(SolverResult::Interrupt(None));
                    }
                }

                if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
                    for instance in &problem.chronicles {
                        Printer::print_chronicle(&instance.chronicle, &problem.context.model);
                    }
                }

                if PRINT_RAW_MODEL.get() {
                    Printer::print_problem(&problem);
                }
                debug_date.print_msg("===== Preprocessing ======");
                aries_planning::chronicles::preprocessing::preprocess(&mut problem);
                debug_date.print_msg("==========================");
                if let Some(interrupter) = &interrupter_2 {
                    if *interrupter.borrow() {
                        return Ok(SolverResult::Interrupt(None));
                    }
                }

                if PRINT_MODEL.get() {
                    println!("OMPAS model at depth {}", depth);
                    for (origin, chronicle) in new_pp
                        .instances
                        .iter()
                        .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
                    {
                        println!("{:?}:\n{}", origin, chronicle)
                    }
                    println!("ARIES model at depth {}", depth);
                    Printer::print_problem(&problem);
                }

                let mut pb = FiniteProblem {
                    model: problem.context.model.clone(),
                    origin: problem.context.origin(),
                    horizon: problem.context.horizon(),
                    chronicles: problem.chronicles.clone(),
                };
                aries_planners::encode::populate_with_task_network(&mut pb, &problem, depth)?;
                if let Some(interrupter) = &interrupter_2 {
                    if *interrupter.borrow() {
                        return Ok(SolverResult::Interrupt(None));
                    }
                }

                Ok(SolverResult::Sol((new_pp, table, pb)))
            })
            .await
            .unwrap();
        let (new_pp, table, pb) = match r? {
            SolverResult::Sol((new_pp, table, pb)) => (new_pp, table, pb),
            SolverResult::Interrupt(None) => return Ok(SolverResult::Interrupt(None)),
            _ => unreachable!(),
        };
        let new_pp = Arc::new(new_pp);
        let table = Arc::new(table);
        let pb = Arc::new(pb);
        if PRINT_PLANNER_OUTPUT.get() {
            debug_date.print_msg(" Populated");
        }

        let (tx, mut rx) = mpsc::unbounded_channel();
        let pb2 = pb.clone();
        let int_2 = interrupter.clone();
        let opt2 = *opt;
        let debug_date2 = debug_date.clone();
        tokio::spawn(async move {
            let r = solve_finite_problem(
                debug_date2,
                pb2,
                &STRATEGIES,
                opt2,
                best_cost - 1,
                int_2,
                Some(tx.clone()),
            )
            .await;
            tx.send((r, true))
        });
        if PRINT_PLANNER_OUTPUT.get() {
            debug_date.print_msg(" Solved");
        }

        'loop_result: while let Some(result) = rx.recv().await {
            let last = result.1;
            let result = result.0.map(|assignment| (pb.clone(), assignment));
            let r = match result {
                SolverResult::Unsat => {
                    if fully_populated {
                        debug_date.print_msg("No solution");
                        return Ok(SolverResult::Unsat);
                    }
                    break 'loop_result;
                    //println!("unsat")
                }
                SolverResult::Sol((fp, (ass, cost))) => {
                    if let Some(cost) = cost {
                        best_cost = cost
                    }
                    if OMPAS_PLANNER_OUTPUT.get() {
                        debug_date.print_msg("  Solution found");
                        debug_date.print_msg(format!(
                            "\n**** Decomposition ****\n\n\
                    {}\n\n\
                    **** Plan ****\n\n\
                    {}",
                            format_hddl_plan(&fp, &ass).unwrap(),
                            format_pddl_plan(&fp, &ass).unwrap(),
                        ));
                    }

                    SolverResult::Sol(PlanResult {
                        ass,
                        fp,
                        pp: new_pp.clone(),
                        table: table.clone(),
                    })
                }
                SolverResult::Timeout(_) => {
                    if OMPAS_PLANNER_OUTPUT.get() {
                        debug_date.print_msg("Timeout");
                    }
                    SolverResult::Timeout(None)
                }
                SolverResult::Interrupt(_) => {
                    if OMPAS_PLAN_OUTPUT.get() {
                        debug_date.print_msg("Interrupt")
                    }
                    SolverResult::Interrupt(None)
                } // continue (increase depth)
            };

            let r = Ok(r);
            if last {
                return r;
            } else {
                if let Some(sender) = intermediate_sender.as_ref() {
                    let _ = sender.send(r);
                }
            }
        }

        pp = new_pp.clone();
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
async fn solve_finite_problem(
    debug_date: DebugDate,
    pb: Arc<FiniteProblem>,
    strategies: &[Strat],
    metric: Option<Metric>,
    cost_upper_bound: IntCst,
    interrupter: Option<PlannerInterrupter>,
    intermediate_sender: Option<
        tokio::sync::mpsc::UnboundedSender<(SolverResult<(Solution, Option<IntCst>)>, bool)>,
    >,
) -> SolverResult<(Solution, Option<IntCst>)> {
    let handle = Handle::current();
    if let Some(interrupter) = &interrupter {
        if *interrupter.borrow() {
            return SolverResult::Interrupt(None);
        }
    }
    if PRINT_INITIAL_PROPAGATION.get() {
        propagate_and_print(&pb);
    }
    let (encoded, pb) = handle
        .spawn_blocking(move || (aries_planners::encode::encode(&pb, metric), pb))
        .await
        .unwrap();
    if let Some(interrupter) = &interrupter {
        if *interrupter.borrow() {
            return SolverResult::Interrupt(None);
        }
    }
    debug_date.print_msg("[Aries] CSP problem encoded");
    let Ok(EncodedProblem {
        mut model,
        objective: metric,
        encoding,
    }) = encoded
    else {
        return SolverResult::Unsat;
    };
    if let Some(metric) = metric {
        model.enforce(metric.le_lit(cost_upper_bound), []);
    }
    let solver = init_solver(model);
    debug_date.print_msg("[Aries] Solver initialized");

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
    if let Some(interrupter) = &interrupter {
        if *interrupter.borrow() {
            return SolverResult::Interrupt(None);
        }
    }
    debug_date.print_msg("[Aries] ParSolver initialized");

    let input_stream = solver.input_stream();
    let interrupt_handle = tokio::spawn(async move {
        if let Some(mut interrupter) = interrupter {
            if interrupter.wait_for(|b| *b == true).await.is_ok() {
                debug_date.print_msg("Interrupt received");
                let _ = input_stream.sender.send(InputSignal::Interrupt);
            }
        }
    });

    let int = intermediate_sender.clone();

    let on_new_solution = move |s: Solution| {
        let cost = metric.map(|metric| s.domain_of(metric).0);
        if let Some(sender) = int.as_ref() {
            let _ = sender.send((SolverResult::Sol((s, cost)), false));
        }
    };

    let join = handle.spawn_blocking(move || {
        debug_date.print_msg("[Aries] Starting solver");

        let result = if let Some(metric) = metric {
            solver.minimize_with(metric, on_new_solution, None)
        } else {
            solver.solve(None)
        };
        debug_date.print_msg("Solver Ended");

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
    });
    let r = join.await.unwrap();
    interrupt_handle.abort();
    r
}
