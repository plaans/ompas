use crate::model::acting_domain::model::NewTask;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::ProcessRef;
use crate::ompas::interface::select_mode::{AriesConfig, Planner, SelectMode};
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::process::task::Selected;
use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::planning::get_update;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::refinement::greedy_select;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::model::get_plan_env;
use crate::planning::planner::ompas_lcp;
use crate::planning::planner::ompas_lcp::OMPASLCPConfig;
use crate::planning::planner::problem::new_problem_chronicle_instance;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

//Returns the method to do.
pub async fn aries_select(
    task_id: usize,
    task: &[LValue],
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    aries_config: AriesConfig,
    acting_manager: ActingManager,
    empty_env: LEnv,
) -> lruntimeerror::Result<Selected> {
    let mut plan_env = get_plan_env(&acting_manager.domain_manager, empty_env).await;

    plan_env.update_context(ModState::new_from_snapshot(state.clone()));
    let opt = match aries_config {
        AriesConfig::Satisfactory => None,
        AriesConfig::Optimality(opt) => Some(opt),
    };
    let st = acting_manager.st.clone();

    let domain: OMPASDomain = acting_manager.domain_manager.get_inner().await;

    let resource_state = acting_manager.resource_manager.get_snapshot(None).await;
    let mut plan_state = state.clone();
    plan_state.absorb(resource_state);
    let tasks = vec![NewTask {
        start: Some(acting_manager.clock_manager.now()),
        args: task.iter().map(|lv| lv.as_cst().unwrap()).collect(),
    }];
    let ep: ExecutionProblem = ExecutionProblem {
        state: plan_state,
        st: st.clone(),
        chronicles: vec![new_problem_chronicle_instance(&st, tasks.clone(), vec![], vec![]).await],
    };

    // //hack
    // for (i, task) in tasks.iter().enumerate() {
    //     if let Some(start) = task.start {
    //         let ch = &mut ep.chronicles[i + 1].instantiated_chronicle;
    //         *ch = ch.clone().instantiate(vec![Instantiation::new(
    //             ch.interval.get_start(),
    //             st.new_cst(start.as_cst().unwrap()),
    //         )]);
    //     }
    // }

    let result = ompas_lcp::run_planner(
        &ep,
        &OMPASLCPConfig {
            state_subscriber_id: None,
            opt,
            domain: Arc::new(domain),
            env: plan_env,
            debug_date: None,
        },
        None,
        None,
    )
    .await;

    let update_pr = |pr: &mut ProcessRef| {
        let ProcessRef::Relative(id, vec) = pr else {
            unreachable!()
        };
        *id = task_id;
        vec.remove(0);
    };

    let selected = if let Some(mut update) = get_update(result) {
        for chronicle in &mut update.acting_models {
            update_pr(&mut chronicle.pr);
            // println!(
            //     "{}; {}\n{}",
            //     chronicle.pr,
            //     chronicle.refinement_label,
            //     chronicle.instantiated_chronicle.to_string()
            // );
        }
        //Remove choices for C_0 interval and task name;
        update.choices.remove(0);
        update.choices.remove(0);
        for choice in &mut update.choices {
            update_pr(&mut choice.process_ref);
        }
        // for choice in &update.choices {
        //     println!("{}:{}", choice.process_ref, choice.choice_inner);
        // }
        //panic!()
        // for choice in &update.choices {
        //     if let ChoiceInner::Refinement(r) = &choice.choice_inner {
        //
        //         return Ok()
        //     }
        // }
        acting_manager.update_acting_tree(update).await;
        let method_id = acting_manager
            .get_last_planned_refinement(&task_id)
            .await
            .unwrap();
        let lv = acting_manager.get_refinement_lv(&method_id).await;
        if candidates.contains(&lv) {
            Selected::PlannerGenerated(
                method_id,
                SelectMode::Planning(Planner::Aries(aries_config)),
            )
        } else {
            greedy_select(candidates, &state, &env)?
        }
    } else {
        greedy_select(candidates, &state, &env)?
    };

    Ok(selected)
}
