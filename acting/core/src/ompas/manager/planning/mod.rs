use crate::model::acting_domain::model::{ActingModel, ModelKind};
use crate::model::acting_domain::OMPASDomain;
use crate::model::chronicle::acting_process_model::{ActingProcessModel, ActingProcessModelLabel};
use crate::model::chronicle::effect::{Effect, EffectOperation};
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::process_ref::{Label, MethodLabel, ProcessRef, RefinementLabel};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::interval::Duration as OMPASDuration;
use crate::ompas::manager::acting::{PlannerReactivity, RefInnerActingManager};
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::ompas::manager::planning::plan_update::*;
use crate::ompas::manager::planning::planner_stat::{
    PlannerStat, PlanningInstanceStat, PlanningStatus,
};
use crate::ompas::manager::planning::problem_update::{ExecutionProblem, PlannerUpdate, VarUpdate};
use crate::ompas::manager::resource::{ResourceManager, WaiterPriority};
use crate::ompas::manager::state::state_update_manager::{StateRule, StateUpdateSubscriber};
use crate::ompas::manager::state::StateManager;
use crate::ompas::scheme::exec::state::ModState;
use crate::planning::planner::encoding::domain::encode_ctx;
use crate::planning::planner::encoding::instance::generate_instances;
use crate::planning::planner::encoding::problem_generation::{
    generate_acting_model_or_empty, ActionParam, PAction,
};
use crate::planning::planner::encoding::{PlannerDomain, PlannerProblem};
use crate::planning::planner::ompas_lcp;
use crate::planning::planner::ompas_lcp::OMPASLCPConfig;
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::planning::planner::solver::{PMetric, PlannerInterruptSender};
use crate::{
    ChronicleDebug, OMPAS_CHRONICLE_DEBUG, OMPAS_DEBUG_CONTINUOUS_PLANNING, OMPAS_PLAN_OUTPUT,
};
use aries::collections::seq::Seq;
use aries::model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries::model::lang::Variable;
use aries::model::Model;
use aries_planners::solver::SolverResult;
use aries_planning::chronicles;
use aries_planning::chronicles::{ChronicleOrigin, FiniteProblem, TaskId, VarLabel};
use itertools::Itertools;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use planner_manager_interface::PlannerManagerInterface;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Write};
use std::mem;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::{mpsc, watch, RwLock};

pub mod acting_var_ref_table;
pub mod plan_update;
pub mod planner_manager_interface;
pub mod planner_stat;
pub mod problem_update;

const PROCESS_PLANNER_MANAGER: &str = "__PROCESS_PLANNER_MANAGER__";
struct PlannerManagerConfig {
    acting_manager: RefInnerActingManager,
    state_manager: StateManager,
    resource_manager: ResourceManager,
    clock_manager: ClockManager,
    domain_manager: DomainManager,
    st: RefSymTable,
    env: LEnv,
    planner_reactivity: PlannerReactivity,
    opt: Option<PMetric>,
    rx_update: mpsc::UnboundedReceiver<PlannerUpdate>,
    stats: Arc<RwLock<PlannerStat>>,
}

pub struct PlannerManager {}

#[derive(Copy, Clone)]
pub struct DebugDate {
    instance: u64,
    instant: SystemTime,
}

impl DebugDate {
    pub fn new(tick: u64) -> Self {
        Self {
            instance: tick,
            instant: SystemTime::now(),
        }
    }

    pub fn print_msg(&self, msg: impl Display) {
        if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
            println!(
                "[({})+{:.3} ms] {}",
                self.instance,
                self.instant.elapsed().unwrap().as_secs_f32() * 1000.0,
                msg
            )
        }
    }
}

pub struct UpdateConfig {
    state_update_subscriber: StateUpdateSubscriber,
    acting_tree_update_subscriber: UnboundedReceiver<PlannerUpdate>,
    planner_reactivity: PlannerReactivity,
}

impl UpdateConfig {
    pub async fn wait_on_update(&mut self) -> Vec<PlannerUpdate> {
        tokio::select! {
            Some(update) = self.acting_tree_update_subscriber.recv() => {
                let updates = vec![update];
                self.get_last_updates(updates, Default::default())
            }
            Some(updated) = self.state_update_subscriber.channel.recv() => {
                let mut sv_updated : HashSet<LValueS> = Default::default();
                for update in updated {
                    sv_updated.insert(update);
                }
                self.get_last_updates(vec![], sv_updated)
            }

        }
    }

    pub fn get_last_updates(
        &mut self,
        mut updates: Vec<PlannerUpdate>,
        mut sv_updated: HashSet<LValueS>,
    ) -> Vec<PlannerUpdate> {
        while let Ok(update) = self.acting_tree_update_subscriber.try_recv() {
            updates.push(update)
        }

        while let Ok(updated) = self.state_update_subscriber.channel.try_recv() {
            for update in updated {
                sv_updated.insert(update);
            }
        }

        if !sv_updated.is_empty() {
            updates.push(PlannerUpdate::StateUpdate(sv_updated.drain().collect()));
        }
        updates
    }
}

impl PlannerManager {
    pub async fn run(
        acting_manager: RefInnerActingManager,
        state_manager: StateManager,
        domain_manager: DomainManager,
        st: RefSymTable,
        env: LEnv,
        planner_reactivity: PlannerReactivity,
        opt: Option<PMetric>,
    ) -> PlannerManagerInterface {
        let (tx_update, rx_update) = mpsc::unbounded_channel();
        let stat: Arc<RwLock<PlannerStat>> = Default::default();
        let pmi = PlannerManagerInterface::new(tx_update, stat.clone());
        let resource_manager = acting_manager.read().await.resource_manager.clone();
        let clock_manager = acting_manager.read().await.clock_manager.clone();
        tokio::spawn(Self::continuous_planning(PlannerManagerConfig {
            acting_manager,
            state_manager,
            resource_manager,
            clock_manager,
            domain_manager: domain_manager,
            st,
            env,
            opt,
            planner_reactivity,
            rx_update,
            stats: stat,
        }));
        pmi
    }

    async fn wait_next_cycle(update_config: &mut UpdateConfig) -> Vec<PlannerUpdate> {
        tokio::time::sleep(Duration::from_secs_f64(
            update_config.planner_reactivity.get_planner_reactivity(),
        ))
        .await;
        update_config.wait_on_update().await
    }

    async fn continuous_planning(config: PlannerManagerConfig) {
        let PlannerManagerConfig {
            acting_manager,
            state_manager,
            resource_manager,
            clock_manager,
            domain_manager,
            st,
            env,
            opt,
            planner_reactivity,
            rx_update,
            stats,
        } = config;

        let domain = domain_manager.get_inner().await;
        let domain = Arc::new(domain);
        let mut process = ProcessInterface::new(
            PROCESS_PLANNER_MANAGER,
            PROCESS_TOPIC_OMPAS,
            LOG_TOPIC_OMPAS,
        )
        .await;

        let mut update_config = UpdateConfig {
            state_update_subscriber: state_manager
                .new_subscriber(StateRule::Specific(vec![]))
                .await,
            acting_tree_update_subscriber: rx_update,
            planner_reactivity: planner_reactivity.clone(),
        };

        let mut last_updates: Option<Vec<PlannerUpdate>> = None;

        let mut instance: u64 = 0;
        'main: loop {
            //Debug
            let updates = match last_updates.take() {
                None => update_config.wait_on_update().await,
                Some(mut last_updates) => {
                    last_updates
                        .append(&mut update_config.get_last_updates(vec![], Default::default()));
                    last_updates
                }
            };

            let debug_date = DebugDate::new(instance);
            debug_date.print_msg("New planning instance");

            let explanation = {
                let mut explanation = format!("Planning instance n°{}\n", debug_date.instance);
                for update in updates {
                    let update: PlannerUpdate = update;
                    match update {
                        PlannerUpdate::VarUpdate(v) => {
                            writeln!(explanation, "- Update of vars {}.", v.format(&st, true))
                                .unwrap();
                        }
                        PlannerUpdate::ProblemUpdate(a) => {
                            writeln!(explanation, "- Planning with new process {a}.").unwrap();
                        }
                        PlannerUpdate::StateUpdate(s) => {
                            writeln!(explanation, "- State Update:").unwrap();
                            for u in s {
                                writeln!(explanation, "\t - {}", u).unwrap();
                            }
                        }
                        PlannerUpdate::Plan => {
                            writeln!(explanation, "- Requested replanning of tree.").unwrap();
                        }
                    }
                }
                explanation
            };

            let mut new_state = state_manager.get_snapshot().await;
            let resource_state = resource_manager
                .get_snapshot(Some(clock_manager.now()))
                .await;
            new_state.absorb(resource_state);

            let mut env = env.clone();
            env.update_context(ModState::new_from_snapshot(new_state.clone()));
            debug_date.print_msg("Getting new state");

            let chronicles = acting_manager.read().await.get_current_chronicles();
            debug_date.print_msg("Getting current chronicles");

            let ep = ExecutionProblem {
                state: new_state,
                st: st.clone(),
                chronicles: chronicles,
            };

            let config = PlannerInstanceConfig {
                id: instance,
                config: OMPASLCPConfig {
                    state_subscriber_id: update_config.state_update_subscriber.id,
                    opt,
                    state_manager: state_manager.clone(),
                    domain: domain.clone(),
                    env,
                    debug_date,
                },
                clock_manager: clock_manager.clone(),
                explanation,
            };

            debug_date.print_msg("Creating new planning instance");
            let mut planner_instance = PlannerInstance::new(ep, config).await;

            tokio::select! {
                Some(PlannerResult {
                    stat,
                    update,
                    debug_date
                }) = planner_instance.wait_on_plan() => {
                    debug_date.print_msg("planning instance terminated");
                    stats.write().await.add_stat(stat);
                    if let Some(update) = update {
                        debug_date.print_msg("Updating Acting tree");
                        acting_manager.write().await.update_acting_tree(update).await;
                        debug_date.print_msg("Acting Tree Updated");
                    }
                }
                _ = process.recv() => {
                    //println!("killing process planner manager");
                    if let Some(pr) = planner_instance.interrupt().await {
                        let stat = pr.stat;
                        stats.write().await.add_stat(stat);
                    };
                    break 'main;
                }
                new_updates = Self::wait_next_cycle(&mut update_config) => {

                    last_updates = Some(new_updates);
                    if let Some(pr) = planner_instance.interrupt().await
                    {
                        let stat = pr.stat;
                        stats.write().await.add_stat(stat);
                        if let Some(update) = pr.update {
                            acting_manager
                                .write()
                                .await
                                .update_acting_tree(update)
                                .await;
                        }
                    };

                }
            }
            instance += 1;
        }
    }
}

pub struct PlannerResult {
    stat: PlanningInstanceStat,
    update: Option<ActingTreeUpdate>,
    debug_date: DebugDate,
}

pub struct PlannerInstanceConfig {
    pub id: u64,
    pub config: OMPASLCPConfig,
    pub clock_manager: ClockManager,
    pub explanation: String,
}

pub struct PlannerInstance {
    _id: u64,
    debug_date: DebugDate,
    _updater: mpsc::UnboundedSender<VarUpdate>,
    interrupter: PlannerInterruptSender,
    plan_receiver: UnboundedReceiver<PlannerResult>,
}

impl PlannerInstance {
    pub async fn interrupt(&mut self) -> Option<PlannerResult> {
        let debug_date = self.debug_date;
        debug_date.print_msg("Sending interruption signal");
        let _ = self.interrupter.send(true);
        debug_date.print_msg("Interruption signal sent");
        self.wait_on_plan().await
    }

    pub async fn wait_on_plan(&mut self) -> Option<PlannerResult> {
        self.plan_receiver.recv().await
    }

    pub async fn new(execution_problem: ExecutionProblem, config: PlannerInstanceConfig) -> Self {
        let mut stat = PlanningInstanceStat {
            id: config.id,
            duration: OMPASDuration::zero(),
            status: PlanningStatus::Unsat,
            seeking_optimal: false,
        };

        let PlannerInstanceConfig {
            id,
            config,
            clock_manager,
            explanation,
        } = config;

        let debug_date = config.debug_date;

        let start = clock_manager.now();

        if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
            println!("Planning for:\n{}", explanation);
        }
        let (_updater, _updated) = mpsc::unbounded_channel();
        let (interrupter, interrupted) = watch::channel(false);
        let (plan_sender, plan_receiver) = mpsc::unbounded_channel();
        let exp = Arc::new(explanation);
        tokio::spawn(async move {
            let debug_date = config.debug_date;
            let result =
                ompas_lcp::run_planner(&execution_problem, &config, |_, _| {}, Some(interrupted))
                    .await;

            let update = if let Ok(solver_result) = result {
                match solver_result {
                    SolverResult::Sol(pr) => {
                        stat.status = PlanningStatus::Sat(stat.seeking_optimal);

                        let choices = extract_choices(&pr);
                        let PlanResult { pp, .. } = &pr;

                        if OMPAS_PLAN_OUTPUT.get() {
                            debug_date.print_msg(format!("Successfully planned for:\n{}", exp));

                            if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::On {
                                for (origin, chronicle) in pp
                                    .instances
                                    .iter()
                                    .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
                                {
                                    println!("{:?}:\n{}", origin, chronicle)
                                }
                            }
                            for choice in &choices {
                                println!("{}:{}", choice.process_ref, choice.choice_inner)
                            }
                        }
                        let new_ams = extract_new_acting_models(&pr);

                        //We update the plan with new acting models and choices extracted from the instanciation of variables of the planner.
                        Some(ActingTreeUpdate {
                            acting_models: new_ams,
                            choices,
                        })
                    }
                    SolverResult::Unsat => {
                        stat.status = PlanningStatus::Unsat;
                        None
                    }
                    SolverResult::Interrupt(_) => {
                        stat.status = PlanningStatus::Interrupted;
                        None
                    }
                    SolverResult::Timeout(_) => {
                        stat.status = PlanningStatus::Timeout;
                        None
                    }
                }
            } else {
                if OMPAS_PLAN_OUTPUT.get() {
                    debug_date.print_msg("No solution found by planner for");
                    if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
                        for (origin, chronicle) in execution_problem
                            .chronicles
                            .iter()
                            .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
                        {
                            println!("{:?}:\n{}", origin, chronicle)
                        }
                    }
                }
                None
            };

            let end = clock_manager.now();
            stat.duration =
                crate::ompas::manager::acting::interval::Interval::new(start, Some(end)).duration();
            //println!("sending update");
            if plan_sender
                .send(PlannerResult {
                    stat,
                    update,
                    debug_date,
                })
                .is_err()
            {
                panic!("error sending plan update");
            }
        });

        Self {
            _id: id,
            debug_date,
            _updater,
            interrupter,
            plan_receiver,
        }
    }

    pub fn update(&self, updates: Vec<VarUpdate>) {
        for update in updates {
            let _ = self._updater.send(update);
        }
    }
}

pub enum FinitePlanningProblem<'a> {
    ExecutionProblem(&'a ExecutionProblem),
    PlannerProblem(&'a PlannerProblem),
}

/// Takes a finite execution model and adds needed chronicles to complete the model for the planner.
/// First it searches tasks that have been refined, and then adds the new Acting Models
pub async fn populate_problem<'a>(
    fpp: FinitePlanningProblem<'a>,
    domain: &OMPASDomain,
    env: &LEnv,
    max_depth: u32,
) -> Result<PlannerProblem, LRuntimeError> {
    let (state, st, new_instances, templates, mut commands, mut methods, mut tasks, mut sf_labels) =
        match fpp {
            FinitePlanningProblem::ExecutionProblem(ep) => {
                let ExecutionProblem {
                    state,
                    st,
                    chronicles,
                } = ep;

                let commands: HashSet<String> = Default::default();
                let methods: HashSet<String> = Default::default();
                let tasks: HashSet<String> = Default::default();
                let sf_labels: HashSet<String> = Default::default();
                (
                    state,
                    st,
                    chronicles.clone(),
                    vec![],
                    commands,
                    methods,
                    tasks,
                    sf_labels,
                )
            }
            FinitePlanningProblem::PlannerProblem(PlannerProblem {
                st,
                instances,
                templates,
                domain,
                state,
            }) => {
                let methods: HashSet<String> = domain.methods.clone().to_set();
                let commands: HashSet<String> = domain.commands.clone().to_set();
                let tasks: HashSet<String> = domain.tasks.clone().to_set();
                let sf_labels: HashSet<String> = domain
                    .sf
                    .iter()
                    .map(|sf| sf.get_label().to_string())
                    .collect();
                (
                    state,
                    st,
                    instances.clone(),
                    templates.clone(),
                    commands,
                    methods,
                    tasks,
                    sf_labels,
                )
            }
        };

    let mut update_problem = |p_actions: &mut Vec<PAction>,
                              cis: &Vec<ChronicleInstance>,
                              (ci, instance_id): (&ChronicleInstance, usize)|
     -> Vec<ChronicleInstance> {
        let chronicle = &ci.instantiated_chronicle;
        // if OMPAS_CHRONICLE_DEBUG.get() >= ChronicleDebug::Full {
        //     println!(
        //         "instantiated:{}\nmodel:{}",
        //         chronicle,
        //         ci.am.chronicle.as_ref().unwrap()
        //     )
        // }
        if ci.origin != ChronicleOrigin::Original {
            let kind = chronicle.get_debug().kind;
            let label = chronicle.get_name()[0].format(st, true);
            if kind == ChronicleKind::Command {
                commands.insert(label);
            } else if kind == ChronicleKind::Method {
                methods.insert(label);
                tasks.insert(chronicle.get_task()[0].format(st, true));
            }
        }
        let mut new = vec![];
        'loop_task: for (task_id, subtask) in chronicle.get_subtasks().iter().enumerate() {
            // If we do not find at least one chronicle to refine the subtask,
            // then we add a new chronicle

            let origin = ChronicleOrigin::Refinement {
                refined: vec![TaskId {
                    instance_id,
                    task_id,
                }],
                template_id: 0,
            };

            if !cis[instance_id..].iter().any(|ci| ci.origin == origin) {
                let mut value: Vec<ActionParam> = vec![];
                for e in &subtask.name {
                    let domain = st.get_domain_of_var(*e);

                    let val = match domain.as_cst() {
                        Some(cst) => ActionParam::Instantiated(cst.clone().into()),
                        None => ActionParam::Uninstantiated(st.format_variable(*e).into()),
                    };
                    value.push(val)
                }

                let subtask_label = subtask.name[0].format(st, true);
                let mut pr = ci.pr.clone();

                let label = subtask.label.unwrap();
                pr.push(label);
                match label {
                    Label::Command(_) => {
                        commands.insert(subtask_label);
                    }
                    Label::Task(_) => {
                        tasks.insert(subtask_label);
                    }
                    Label::SyntheticTask(id) => {
                        let task_template = &chronicle.sub_chronicles[id];

                        for (i, method) in task_template.methods.iter().enumerate() {
                            let mut pr = pr.clone();
                            let refinement_label = RefinementLabel {
                                refinement_id: 0,
                                method_label: MethodLabel::Possibility(i),
                            };
                            pr.push(Label::Refinement(refinement_label));
                            new.push(ChronicleInstance {
                                instantiated_chronicle: method
                                    .clone()
                                    .instantiate_and_clean(Default::default()),
                                generated: true,
                                origin: origin.clone(),
                                am: ActingModel {
                                    lv: LValue::Nil,
                                    lv_om: LValue::Nil,
                                    lv_expanded: None,
                                    runtime_info: Default::default(),
                                    chronicle: Some(method.clone()),
                                },
                                pr,
                                refinement_label,
                            })
                        }
                        continue 'loop_task;
                    }
                    _ => unreachable!(),
                };

                p_actions.insert(
                    0,
                    PAction {
                        args: value,
                        origin,
                        pr,
                    },
                )
            }
        }
        for effect in chronicle.get_effects() {
            let label = effect.sv[0].format(st, true);
            sf_labels.insert(label);
        }

        for condition in chronicle.get_conditions() {
            sf_labels.insert(condition.sv[0].format(st, true));
        }
        new
    };

    let mut p_actions = vec![];
    let mut instances = new_instances;
    let mut new = vec![];
    for (instance_id, ci) in instances.iter().enumerate() {
        let mut r = update_problem(&mut p_actions, &instances, (ci, instance_id));
        new.append(&mut r);
    }
    let mut add_instance =
        |p_actions: &mut Vec<PAction>, cis: &mut Vec<ChronicleInstance>, ci: ChronicleInstance| {
            let mut queue = VecDeque::new();
            queue.push_back(ci);
            while let Some(mut ci) = queue.pop_front() {
                update_problem(p_actions, cis, (&ci, cis.len()))
                    .drain(..)
                    .for_each(|ci| queue.push_back(ci));
                ci.instantiated_chronicle.sub_chronicles = vec![];
                cis.push(ci);
            }
        };

    for ci in new {
        add_instance(&mut p_actions, &mut instances, ci)
    }

    for _ in 0..max_depth {
        let mut new_p_actions = vec![];
        for action in p_actions {
            let args = &action.args;
            let cst_args: Vec<_> = args
                .iter()
                .map(|ap| match ap {
                    ActionParam::Instantiated(lv) => lv.as_cst(),
                    ActionParam::Uninstantiated(_) => None,
                })
                .collect();
            if let Some(task) = domain.tasks.get(args[0].lvalues().to_string().as_str()) {
                let params = task.get_parameters().get_labels();
                assert_eq!(params.len(), args.len() - 1);

                match task.get_model(&ModelKind::PlanModel) {
                    Some(model) => {
                        let model_lambda: LLambda = model.try_into().expect("");

                        let am: ActingModel = match domain
                            .acting_model_collection
                            .as_ref()
                            .and_then(|amc| amc.try_instantiate_task(&cst_args, st))
                        {
                            Some(am) => am,
                            None => {
                                generate_acting_model_or_empty(
                                    &model_lambda,
                                    args,
                                    None,
                                    task.get_parameters(),
                                    st,
                                    env,
                                    ChronicleKind::Task,
                                )
                                .await?
                            }
                        };
                        let mut pr = action.pr;
                        pr.push(Label::AbstractModel);

                        let instance = ChronicleInstance {
                            instantiated_chronicle: am.get_clean_instantiated_chronicle().unwrap(),
                            generated: true,
                            origin: action.origin,
                            am,
                            pr,
                            refinement_label: RefinementLabel {
                                refinement_id: 0,
                                method_label: MethodLabel::Possibility(0),
                            },
                        };

                        add_instance(&mut new_p_actions, &mut instances, instance);
                    }
                    None => {
                        for (i, m_label) in task.get_methods().iter().enumerate() {
                            //methods.insert(m_label.to_string());
                            let method = domain.get_methods().get(m_label).unwrap();
                            let method_lambda: LLambda = method.get_body().try_into().expect("");

                            let mut p_method: PAction = action.clone();
                            p_method.args[0] = ActionParam::Instantiated(m_label.clone().into());
                            for param in &method.parameters.get_labels()
                                [task.get_parameters().inner().len()..]
                            {
                                p_method
                                    .args
                                    .push(ActionParam::Uninstantiated(param.to_string().into()))
                            }

                            let args = &p_method.args;

                            let cst_args: Vec<_> = args
                                .iter()
                                .map(|ap| match ap {
                                    ActionParam::Instantiated(lv) => lv.as_cst(),
                                    ActionParam::Uninstantiated(_) => None,
                                })
                                .collect();

                            let am: ActingModel = match domain
                                .acting_model_collection
                                .as_ref()
                                .and_then(|amc| amc.try_instantiate_method(&cst_args, st))
                            {
                                Some(am) => am,
                                None => {
                                    generate_acting_model_or_empty(
                                        &method_lambda,
                                        args,
                                        Some(&action.args),
                                        method.get_parameters(),
                                        st,
                                        env,
                                        ChronicleKind::Method,
                                    )
                                    .await?
                                }
                            };
                            let mut pr = p_method.pr;
                            let method_id = RefinementLabel {
                                refinement_id: 0,
                                method_label: MethodLabel::Possibility(i),
                            };
                            pr.push(Label::Refinement(method_id));
                            let instance = ChronicleInstance {
                                instantiated_chronicle: am
                                    .get_clean_instantiated_chronicle()
                                    .unwrap(),
                                generated: true,
                                origin: p_method.origin,
                                am,
                                pr,
                                refinement_label: RefinementLabel {
                                    refinement_id: 0,
                                    method_label: MethodLabel::Possibility(0),
                                },
                            };

                            add_instance(&mut new_p_actions, &mut instances, instance);
                        }
                    }
                }
            } else if let Some(command) = domain
                .commands
                .get(action.args[0].lvalues().to_string().as_str())
            {
                let model_lambda: LLambda = command
                    .get_model(&ModelKind::PlanModel)
                    .unwrap()
                    .try_into()
                    .expect("");

                let am: ActingModel = match domain
                    .acting_model_collection
                    .as_ref()
                    .and_then(|amc| amc.try_instantiate_command(&cst_args, st))
                {
                    Some(am) => am,
                    None => {
                        generate_acting_model_or_empty(
                            &model_lambda,
                            args,
                            None,
                            command.get_parameters(),
                            st,
                            env,
                            ChronicleKind::Command,
                        )
                        .await?
                    }
                };
                let mut pr = action.pr;
                pr.push(Label::AbstractModel);

                let instance = ChronicleInstance {
                    instantiated_chronicle: am.get_clean_instantiated_chronicle().unwrap(),
                    generated: true,
                    origin: action.origin,
                    am,
                    pr,
                    refinement_label: RefinementLabel {
                        refinement_id: 0,
                        method_label: MethodLabel::Possibility(0),
                    },
                };

                add_instance(&mut new_p_actions, &mut instances, instance);
            }
        }
        p_actions = new_p_actions;
    }

    let sf = domain
        .get_state_functions()
        .iter()
        .filter_map(|(k, v)| {
            if sf_labels.contains(k) {
                Some(v.clone())
            } else {
                None
            }
        })
        .collect();

    let methods: Vec<_> = methods.to_vec();

    let commands: Vec<_> = commands.to_vec();

    let tasks: Vec<_> = tasks.to_vec();

    let mut pp = PlannerProblem {
        st: st.clone().clone(),
        instances,
        templates,
        domain: PlannerDomain {
            sf,
            methods,
            tasks,
            commands,
        },
        state: state.clone(),
    };

    initialize_root_chronicle(&mut pp);

    Ok(pp)
}

fn initialize_root_chronicle(pp: &mut PlannerProblem) {
    let state = &pp.state;
    let init_ch = &pp.instances[0].instantiated_chronicle;
    let st = pp.st.clone();
    let present_sf: Vec<&str> = pp.domain.sf.iter().map(|sf| sf.get_label()).collect();

    let mut effects = vec![];

    struct ActiveEffect {
        sv: String,
        start: f64,
        end: Option<f64>,
    }

    // List of sv that are currently modified in the model
    let mut active_effects: Vec<ActiveEffect> = vec![];

    for chronicle in pp.instances.iter().map(|i| &i.instantiated_chronicle) {
        for e in chronicle.get_effects() {
            let start_domain = st.get_domain_of_var(e.get_start());
            if start_domain.is_constant() {
                active_effects.push(ActiveEffect {
                    sv: e.sv.format(&st, true),
                    start: start_domain.as_cst().unwrap().as_float().unwrap(),
                    end: st
                        .get_domain_of_var(e.get_end())
                        .as_cst()
                        .map(|c| c.as_float().unwrap()),
                })
            }
        }
    }

    /*
    Initialisation of static state variables
     */
    //println!("state: ");
    //We suppose for the moment that all args of state variable are objects
    'loop_fact: for (key, fact) in state.get_state(None).inner {
        let sv: Vec<VarId> = match key {
            LValueS::List(vec) => {
                let sf = vec[0].to_string();
                if present_sf.contains(&sf.as_str()) {
                    vec.iter()
                        .map(|lv| st.new_cst(lv.as_cst().unwrap()))
                        .collect()
                } else {
                    continue 'loop_fact;
                }
            }
            LValueS::Symbol(sf) => {
                if present_sf.contains(&sf.as_str()) {
                    vec![st.new_symbol(sf)]
                } else {
                    continue 'loop_fact;
                }
            }
            _ => panic!("state variable is either a symbol or a list of symbols"),
        };
        let value = st.new_cst(fact.value.as_cst().unwrap());
        let t = match fact.date {
            None => init_ch.interval.get_start(),
            Some(t) => st.new_cst(t.as_cst().unwrap()),
        };
        effects.push(Effect {
            interval: Interval::new_instantaneous(t),
            sv,
            operation: EffectOperation::assign(value),
        });
    }

    let init_ch = &mut pp.instances[0].instantiated_chronicle;

    'loop_effect: for effect in effects {
        let effect_date = st
            .get_domain_of_var(effect.get_start())
            .as_cst()
            .unwrap_or_else(|| panic!("{}", effect.format(&st, true)))
            .as_float()
            .unwrap();
        let sv = effect.sv.format(&st, true);
        for ae in &active_effects {
            if sv == ae.sv
                && effect_date >= ae.start
                && match &ae.end {
                    None => true,
                    Some(end) => &effect_date <= end,
                }
            {
                continue 'loop_effect;
            }
        }

        init_ch.add_effect(effect)
    }
}

/// Encode the chronicles in the aries format in order to then call the planner lcp.
pub fn encode(pp: &PlannerProblem) -> anyhow::Result<(chronicles::Problem, ActingVarRefTable)> {
    let mut table = ActingVarRefTable::default();
    let domain = &pp.domain;
    let st = &pp.st;
    let mut context = encode_ctx(st, domain, &pp.state.instance)?;

    let chronicles = generate_instances(&mut context, &mut table, &pp.instances)?;

    Ok((
        chronicles::Problem {
            context,
            templates: vec![],
            chronicles,
        },
        table,
    ))
}

pub fn extract_new_acting_models(pr: &PlanResult) -> Vec<ChronicleInstance> {
    pr.pp
        .instances
        .iter()
        .filter(|c| c.generated)
        .cloned()
        .map(|mut ci| {
            let empty = Chronicle::new("", ChronicleKind::Method, RefSymTable::default());
            let ch = mem::replace(&mut ci.instantiated_chronicle, empty);
            ci.am.chronicle = Some(ch);
            ci
        })
        .collect()
}

pub struct ActingPlanResult {
    pub instances: Vec<ChronicleInstance>,
    pub table: ActingVarRefTable,
    pub assignments: Arc<SavedAssignment>,
    pub finite_problem: Arc<FiniteProblem>,
}

pub fn extract_choices(pr: &PlanResult) -> Vec<Choice> {
    let PlanResult { ass, fp, pp, table } = pr;
    let model = &fp.model;
    let mut choices = vec![];
    let mut resource_accesses: HashMap<String, Vec<(ProcessRef, ChoiceAcquire)>> =
        Default::default();

    let var_id_as_cst = |st: &RefSymTable, var_id: VarId| match st.var_as_cst(var_id) {
        Some(cst) => cst,
        None => get_var_as_cst(table, ass, model, var_id),
    };

    //We extract only present chronicles
    for instance in pp.instances.iter().filter(|c| {
        let presence = c.instantiated_chronicle.get_presence();
        let cst = get_var_as_cst(table, ass, model, presence);
        //println!("{cst}");
        Cst::Bool(true) == cst
    }) {
        let pr = instance.pr.clone();
        let chronicle = &instance.instantiated_chronicle;
        let st = &chronicle.st;

        let start = var_id_as_cst(st, chronicle.interval.get_start());

        let end = var_id_as_cst(st, chronicle.interval.get_end());

        let name: Vec<Cst> = chronicle
            .get_name()
            .iter()
            .map(|var_id| var_id_as_cst(st, *var_id))
            .collect();

        choices.push(Choice::new(
            pr.clone(),
            ChoiceRefinement {
                name,
                start,
                end,
                refinement_label: instance.refinement_label,
            },
        ));

        'choice: for (label, binding) in chronicle.get_all_acting_process_models() {
            let mut pr = pr.clone();
            let ActingProcessModelLabel::Label(label) = label else {
                todo!()
            };
            pr.push(*label);
            let choice: ChoiceInner = match binding {
                ActingProcessModel::Arbitrary(a) => {
                    let val = var_id_as_cst(st, a.var_id);

                    ChoiceInner::Arbitrary(ChoiceArbitrary { val })
                }
                ActingProcessModel::Action(action) => {
                    let name: Vec<Cst> = action
                        .task
                        .name
                        .iter()
                        .map(|var_id| var_id_as_cst(st, *var_id))
                        .collect();

                    let start = var_id_as_cst(st, action.task.interval.get_start());

                    let end = var_id_as_cst(st, action.task.interval.get_end());

                    ChoiceInner::SubTask(ChoiceSubTask { name, start, end })
                }
                ActingProcessModel::Resource(a) => {
                    let resource = var_id_as_cst(st, a.resource);
                    let quantity = var_id_as_cst(st, a.quantity);
                    let request = var_id_as_cst(st, a.request);
                    let s_acq = var_id_as_cst(st, a.acquisition.get_start());
                    let e_acq = var_id_as_cst(st, a.acquisition.get_end());

                    let raw_acquire = ChoiceAcquire {
                        resource,
                        quantity,
                        request,
                        s_acq,
                        e_acq,
                        priority: WaiterPriority::Planner(0),
                    };

                    match resource_accesses.get_mut(&raw_acquire.resource.to_string()) {
                        None => {
                            resource_accesses
                                .insert(raw_acquire.resource.to_string(), vec![(pr, raw_acquire)]);
                        }
                        Some(vec) => {
                            vec.push((pr, raw_acquire));
                        }
                    };
                    continue 'choice;
                }
            };

            choices.push(Choice::new(pr, choice));
        }
    }

    for (_, mut accesses) in resource_accesses {
        accesses
            .drain(..)
            .sorted_by(|(_, a), (_, b)| {
                a.s_acq
                    .as_float()
                    .unwrap()
                    .total_cmp(&b.s_acq.as_float().unwrap())
            })
            .enumerate()
            .for_each(|(id, (pr, mut ra))| {
                ra.priority = WaiterPriority::Planner(id);
                choices.push(Choice::new(pr, ra));
            });
    }

    choices
}

pub fn get_var_as_cst(
    table: &ActingVarRefTable,
    ass: &Arc<SavedAssignment>,
    model: &Model<VarLabel>,
    var: VarId,
) -> Cst {
    match table
        .get_var(var)
        .unwrap_or_else(|| panic!("{var} has no binding in the planner"))
    {
        Variable::Bool(b) => {
            let lit = b.true_lit();
            let value = ass
                .value(lit)
                .unwrap_or_else(|| panic!("{} has no value in the plan", var));
            //println!("{:?}, {:?}, value: {}", b, lit, value);
            Cst::Bool(value)
        }
        Variable::Int(i) => Cst::Int(ass.var_domain(*i).lb as i64),
        Variable::Fixed(f) => Cst::Float(ass.f_domain(*f).lb() as f64),
        Variable::Sym(s) => {
            let sym = ass.sym_domain_of(*s).into_singleton().unwrap();
            let value = model.get_symbol(sym);
            Cst::Symbol(value.to_string())
        }
    }
}
