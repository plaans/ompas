use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::model::add_domain_symbols;
use crate::model::chronicle::acting_process_model::{ActingProcessModel, ActingProcessModelLabel};
use crate::model::chronicle::effect::Effect;
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::ChronicleKind;
use crate::model::process_ref::{Label, MethodLabel, ProcessRef, RefinementLabel};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::RefInnerActingManager;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::ompas::manager::planning::plan_update::*;
use crate::ompas::manager::planning::problem_update::{ExecutionProblem, PlannerUpdate, VarUpdate};
use crate::ompas::manager::resource::{ResourceManager, WaiterPriority};
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::manager::state::StateManager;
use crate::ompas::scheme::exec::state::ModState;
use crate::planning::planner::encoding::domain::encode_ctx;
use crate::planning::planner::encoding::instance::generate_instances;
use crate::planning::planner::encoding::problem_generation::{
    convert_into_chronicle_instance, ActionParam, PAction,
};
use crate::planning::planner::encoding::{PlannerDomain, PlannerProblem};
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::planning::planner::solver::{run_planner, PMetric};
use crate::{
    ChronicleDebug, OMPAS_CHRONICLE_DEBUG_ON, OMPAS_DEBUG_CONTINUOUS_PLANNING, OMPAS_PLAN_OUTPUT_ON,
};
use aries::collections::seq::Seq;
use aries::model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries::model::lang::Variable;
use aries::model::Model;
use aries_planning::chronicles;
use aries_planning::chronicles::{ChronicleOrigin, FiniteProblem, TaskId, VarLabel};
use futures::future::abortable;
use itertools::Itertools;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use planner_manager_interface::PlannerManagerInterface;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot};

pub mod acting_var_ref_table;
pub mod plan_update;
pub mod planner_manager_interface;
pub mod problem_update;

const PROCESS_PLANNER_MANAGER: &str = "__PROCESS_PLANNER_MANAGER__";
struct PlannerManagerConfig {
    acting_manager: RefInnerActingManager,
    state_manager: StateManager,
    resource_manager: ResourceManager,
    clock_manager: ClockManager,
    domain: OMPASDomain,
    st: RefSymTable,
    env: LEnv,
    opt: Option<PMetric>,
    rx_update: mpsc::UnboundedReceiver<PlannerUpdate>,
}

pub struct PlannerManager {}

impl PlannerManager {
    pub async fn run(
        acting_manager: RefInnerActingManager,
        state_manager: StateManager,
        domain: OMPASDomain,
        st: RefSymTable,
        env: LEnv,
        opt: Option<PMetric>,
    ) -> PlannerManagerInterface {
        let (tx_update, rx_update) = mpsc::unbounded_channel();

        let pmi = PlannerManagerInterface::new(tx_update);
        let resource_manager = acting_manager.read().await.resource_manager.clone();
        let clock_manager = acting_manager.read().await.clock_manager.clone();
        tokio::spawn(Self::continuous_planning(PlannerManagerConfig {
            acting_manager,
            state_manager,
            resource_manager,
            clock_manager,
            domain,
            st,
            env,
            opt,
            rx_update,
        }));
        pmi
    }

    async fn wait_plan(
        plan_receiver: &mut Option<oneshot::Receiver<ActingTreeUpdate>>,
    ) -> Option<ActingTreeUpdate> {
        if let Some(plan_receiver) = plan_receiver {
            match plan_receiver.await {
                Ok(pu) => Some(pu),
                Err(_) => None,
            }
        } else {
            None
        }
    }

    fn interrupt(interrupter: &mut Option<oneshot::Sender<bool>>) {
        let interrupter = interrupter.take();
        if let Some(interrupter) = interrupter {
            let _ = interrupter.send(true);
        }
    }

    async fn continuous_planning(config: PlannerManagerConfig) {
        let PlannerManagerConfig {
            acting_manager,
            state_manager,
            resource_manager,
            clock_manager,
            domain,
            st,
            env,
            opt,
            mut rx_update,
        } = config;

        add_domain_symbols(&st, &domain);
        let mut process = ProcessInterface::new(
            PROCESS_PLANNER_MANAGER,
            PROCESS_TOPIC_OMPAS,
            LOG_TOPIC_OMPAS,
        )
        .await;

        let mut planning = false;
        let mut interrupter: Option<oneshot::Sender<bool>> = None;
        let mut _updater: Option<mpsc::UnboundedSender<VarUpdate>> = None;
        let mut plan_receiver: Option<oneshot::Receiver<ActingTreeUpdate>> = None;

        let mut state_update_subscriber = state_manager.new_subscriber(StateRule::All).await;

        let mut clock = clock_manager.subscribe_to_clock().await;

        //TODO: Remove
        let mut state: Option<WorldStateSnapshot>;
        'main: loop {
            tokio::select! {
                Some(pu) = Self::wait_plan(&mut plan_receiver) => {
                    plan_receiver = None;
                    interrupter = None;
                    _updater = None;
                    acting_manager.write().await.update_acting_tree(pu).await;
                }
                _ = process.recv() => {
                    Self::interrupt(&mut interrupter);
                    break 'main;
                }
                Ok(_) = clock.changed() => {

                        let now = clock_manager.now();
                        let tick = *clock.borrow();
                        let mut updates = vec![];
                        while let Ok(update) = rx_update.try_recv() {
                            updates.push(update)
                        }
                        if let Ok(state_update) = state_update_subscriber.channel.try_recv() {
                            updates.push(PlannerUpdate::StateUpdate(state_update))
                        }
                        if !updates.is_empty() {
                        //Debug
                        let explanation = {
                            let mut explanation = format!("Planning tick n°{tick}\n");
                            for update in updates {
                                let update: PlannerUpdate = update;
                                match update {
                                    PlannerUpdate::VarUpdate(v) => {
                                        writeln!(explanation, "- Update of vars {}.", v.format(&st, true)).unwrap();
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
                        if planning {
                            Self::interrupt(&mut interrupter);
                        }

                        let mut new_state = state_manager.get_snapshot().await;
                        let resource_state = resource_manager.get_snapshot(Some(now)).await;
                        new_state.absorb(resource_state);
                        state = Some(new_state);
                        //}

                        let ep = ExecutionProblem {
                            state: state.unwrap(),
                            chronicles: acting_manager.read().await.get_current_chronicles(),
                        };
                        let mut env = env.clone();
                        env.update_context(ModState::new_from_snapshot(ep.state.clone()));

                        let pp: PlannerProblem = populate_problem(&domain, &env, &st, ep).await.unwrap();
                        //std::process::exit(0);

                        if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                            for (origin, chronicle) in pp
                                .instances
                                .iter()
                                .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
                            {
                                println!("{:?}:\n{}", origin, chronicle)
                            }
                        }

                        let rule = StateRule::Specific(pp.domain.sf.iter().map(|sf|sf.get_label().into()).collect());
                        state_manager.update_subscriber_rule(&state_update_subscriber.id, rule).await;

                        let (problem, table) = encode(&pp).await.unwrap();
                        /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                                for instance in &problem.chronicles {
                                    Printer::print_chronicle(&instance.chronicle, &problem.context.model);
                                }
                            }*/

                        let PlannerInstance {
                            _updater: u,
                            interrupter: i,
                            plan_receiver: p,
                        } = PlannerInstance::new(pp, problem, table, opt,explanation );
                        _updater = Some(u);
                        interrupter = Some(i);
                        plan_receiver = Some(p);
                        planning = true;
                    }
                }
            }
        }
    }
}

pub struct PlannerInstance {
    _updater: mpsc::UnboundedSender<VarUpdate>,
    interrupter: oneshot::Sender<bool>,
    plan_receiver: oneshot::Receiver<ActingTreeUpdate>,
}

impl PlannerInstance {
    pub fn new(
        planner_problem: PlannerProblem,
        problem: chronicles::Problem,
        table: ActingVarRefTable,
        opt: Option<PMetric>,
        explanation: String,
    ) -> Self {
        if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
            println!("Planning for:\n{}", explanation);
        }
        let (_updater, _updated) = mpsc::unbounded_channel();
        let (interrupter, interrupted) = oneshot::channel();
        let (plan_sender, plan_receiver) = oneshot::channel();
        let exp = Arc::new(explanation);
        let exp_2 = exp.clone();

        let (planner, handle) = abortable(tokio::spawn(async move {
            let result = run_planner(problem, opt);
            //println!("{}", format_partial_plan(&pb, &x)?);

            if let Ok(Some(pr)) = result {
                //result::print_chronicles(&pr);
                let PlanResult { ass, fp } = pr;

                let choices = extract_choices(&table, &ass, &fp.model, &planner_problem);

                if OMPAS_PLAN_OUTPUT_ON.get() {
                    println!("Successfully planned for:\n{}", exp);

                    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                        for (origin, chronicle) in planner_problem
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
                let new_ams = extract_new_acting_models(&table, &ass, &fp.model, planner_problem);

                //We update the plan with new acting models and choices extracted from the instanciation of variables of the planner.
                Some(ActingTreeUpdate {
                    acting_models: new_ams,
                    choices,
                })
            } else {
                if OMPAS_PLAN_OUTPUT_ON.get() {
                    println!("Successfully planned for:\n{}", exp);
                    println!("No solution found by planner for");
                    //println!("{exp}");
                    if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                        for (origin, chronicle) in planner_problem
                            .instances
                            .iter()
                            .map(|i| (i.origin.clone(), &i.instantiated_chronicle))
                        {
                            println!("{:?}:\n{}", origin, chronicle)
                        }
                    }
                }
                None
            }
        }));

        tokio::spawn(async move {
            let mut process =
                ProcessInterface::new("__PROCESS__ARIES__", PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS)
                    .await;

            tokio::select! {
                _ = process.recv() => {

                }
                _ = interrupted => {
                    handle.abort();
                    if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
                        println!("Interrupted planning for: \n{}", exp_2);
                    }
                }
                Ok(Ok(Some(pu))) = planner => {
                    if plan_sender.send(pu).is_err() {
                        panic!("error sending plan update");
                    }
                }
            }
        });

        Self {
            _updater,
            interrupter,
            plan_receiver,
        }
    }

    pub async fn wait(self) -> ActingTreeUpdate {
        self.plan_receiver.await.unwrap()
    }

    pub fn interrupt(self) {
        let _ = self.interrupter.send(true);
    }

    pub fn update(&self, updates: Vec<VarUpdate>) {
        for update in updates {
            let _ = self._updater.send(update);
        }
    }
}

/// Takes a finite execution model and adds needed chronicles to complete the model for the planner.
/// First it searches tasks that have been refined, and then adds the new Acting Models
pub async fn populate_problem(
    domain: &OMPASDomain,
    env: &LEnv,
    st: &RefSymTable,
    ep: ExecutionProblem,
) -> Result<PlannerProblem, LRuntimeError> {
    let ExecutionProblem { state, chronicles } = ep;

    let mut names: HashSet<String> = Default::default();
    let mut tasks: HashSet<String> = Default::default();
    let mut sf_labels: HashSet<String> = Default::default();

    let mut update_problem = |p_actions: &mut Vec<PAction>,
                              cis: &Vec<ChronicleInstance>,
                              ci: &ChronicleInstance,
                              instance_id: usize| {
        let chronicle = &ci.instantiated_chronicle;
        /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
            println!(
                "instantiated:{}\nmodel:{}",
                chronicle,
                ci.am.chronicle.as_ref().unwrap()
            )
        }*/
        if ci.origin != ChronicleOrigin::Original {
            names.insert(chronicle.get_name()[0].format(st, true));
            tasks.insert(chronicle.get_task()[0].format(st, true));
        }
        for (task_id, subtask) in chronicle.get_subtasks().iter().enumerate() {
            // If we do not find at least one chronicle to refine the subtask,
            // then we add a new chronicle

            let origin = ChronicleOrigin::Refinement {
                refined: vec![TaskId {
                    instance_id,
                    task_id,
                }],
                template_id: 0,
            };

            if !cis.iter().any(|ci| ci.origin == origin) {
                let mut value: Vec<ActionParam> = vec![];
                for e in &subtask.name {
                    let domain = st.get_domain_of_var(e);

                    let val = match domain.as_cst() {
                        Some(cst) => ActionParam::Instantiated(cst.clone().into()),
                        None => ActionParam::Uninstantiated(st.format_variable(e).into()),
                    };
                    value.push(val)
                }

                let mut pr = ci.pr.clone();
                pr.push(subtask.label.unwrap());

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
    };

    let mut p_actions = vec![];
    for (i, instance) in chronicles.iter().enumerate() {
        update_problem(&mut p_actions, &chronicles, instance, i)
    }
    let mut instances: Vec<ChronicleInstance> = chronicles;

    while let Some(action) = p_actions.pop() {
        let tps = &action.args;
        if let Some(task) = domain.tasks.get(tps[0].lvalues().to_string().as_str()) {
            let params = task.get_parameters().get_labels();
            assert_eq!(params.len(), tps.len() - 1);

            match task.get_model(&ModelKind::PlanModel) {
                Some(model) => {
                    let model_lambda: LLambda = model.try_into().expect("");

                    let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                        &model_lambda,
                        action,
                        None,
                        task.get_parameters(),
                        st,
                        env,
                        ChronicleKind::Task,
                    )
                    .await?;
                    instance.pr.push(Label::AbstractModel);

                    update_problem(&mut p_actions, &vec![], &instance, instances.len());

                    instances.push(instance)
                }
                None => {
                    for (i, m_label) in task.get_methods().iter().enumerate() {
                        //methods.insert(m_label.to_string());
                        let method = domain.get_methods().get(m_label).unwrap();
                        let method_lambda: LLambda = method.get_body().try_into().expect("");

                        let mut p_method: PAction = action.clone();
                        p_method.args[0] = ActionParam::Instantiated(m_label.clone().into());
                        for param in
                            &method.parameters.get_labels()[task.get_parameters().inner().len()..]
                        {
                            p_method
                                .args
                                .push(ActionParam::Uninstantiated(param.to_string().into()))
                        }
                        let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                            &method_lambda,
                            p_method,
                            Some(&action.args),
                            method.get_parameters(),
                            st,
                            env,
                            ChronicleKind::Method,
                        )
                        .await?;
                        let method_id = RefinementLabel {
                            refinement_id: 0,
                            method_label: MethodLabel::Possibility(i),
                        };
                        instance.pr.push(Label::Refinement(method_id));
                        update_problem(&mut p_actions, &vec![], &instance, instances.len());

                        instances.push(instance);
                    }
                }
            }
        } else if let Some(command) = domain
            .commands
            .get(action.args[0].lvalues().to_string().as_str())
        {
            //commands.insert(command.get_label().to_string());
            let model_lambda: LLambda = command
                .get_model(&ModelKind::PlanModel)
                .unwrap()
                .try_into()
                .expect("");

            let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                &model_lambda,
                action,
                None,
                command.get_parameters(),
                st,
                env,
                ChronicleKind::Command,
            )
            .await?;
            instance.pr.push(Label::AbstractModel);

            update_problem(&mut p_actions, &vec![], &instance, instances.len());

            instances.push(instance)
        }
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

    let methods: Vec<_> = names
        .intersection(&domain.get_methods().keys().cloned().to_set())
        .cloned()
        .collect();

    let commands: Vec<_> = tasks
        .intersection(&domain.get_commands().keys().cloned().to_set())
        .cloned()
        .collect();

    let tasks: Vec<_> = tasks
        .intersection(&domain.get_tasks().keys().cloned().to_set())
        .cloned()
        .collect();

    let mut pp = PlannerProblem {
        st: st.clone(),
        instances,
        templates: vec![],
        domain: PlannerDomain {
            sf,
            methods,
            tasks,
            commands,
        },
        state,
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
            let start_domain = st.get_domain_of_var(&e.get_start());
            if start_domain.is_constant() {
                active_effects.push(ActiveEffect {
                    sv: e.sv.format(&st, true),
                    start: start_domain.as_cst().unwrap().as_float().unwrap(),
                    end: st
                        .get_domain_of_var(&e.get_end())
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
            value,
        });
    }

    let init_ch = &mut pp.instances[0].instantiated_chronicle;

    'loop_effect: for effect in effects {
        let effect_date = st
            .get_domain_of_var(&effect.get_start())
            .as_cst()
            .unwrap()
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
pub async fn encode(
    pp: &PlannerProblem,
) -> anyhow::Result<(chronicles::Problem, ActingVarRefTable)> {
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

pub fn extract_new_acting_models(
    _table: &ActingVarRefTable,
    _ass: &Arc<SavedAssignment>,
    _model: &Model<VarLabel>,
    mut pp: PlannerProblem,
) -> Vec<ChronicleInstance> {
    pp.instances.drain(..).filter(|c| c.generated).collect()
}

pub struct ActingPlanResult {
    pub instances: Vec<ChronicleInstance>,
    pub table: ActingVarRefTable,
    pub assignments: Arc<SavedAssignment>,
    pub finite_problem: Arc<FiniteProblem>,
}

pub fn extract_choices(
    table: &ActingVarRefTable,
    ass: &Arc<SavedAssignment>,
    model: &Model<VarLabel>,
    pp: &PlannerProblem,
) -> Vec<Choice> {
    let mut choices = vec![];
    let mut resource_accesses: HashMap<String, Vec<(ProcessRef, ChoiceAcquire)>> =
        Default::default();

    let var_id_as_cst = |st: &RefSymTable, var_id: &VarId| match st.var_as_cst(var_id) {
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

        let start = var_id_as_cst(st, &chronicle.interval.get_start());

        let end = var_id_as_cst(st, &chronicle.interval.get_end());

        let name: Vec<Cst> = chronicle
            .get_name()
            .iter()
            .map(|var_id| var_id_as_cst(st, var_id))
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

        'choice: for (label, binding) in &chronicle.acting_process_models.inner {
            let mut pr = pr.clone();
            let ActingProcessModelLabel::Label(label) = label else {
                todo!()
            };
            pr.push(*label);
            let choice: ChoiceInner = match binding {
                ActingProcessModel::Arbitrary(a) => {
                    let val = var_id_as_cst(st, &a.var_id);

                    ChoiceInner::Arbitrary(ChoiceArbitrary { val })
                }
                ActingProcessModel::Action(action) => {
                    let name: Vec<Cst> = action
                        .task
                        .name
                        .iter()
                        .map(|var_id| var_id_as_cst(st, var_id))
                        .collect();

                    let start = var_id_as_cst(st, &action.task.interval.get_start());

                    let end = var_id_as_cst(st, &action.task.interval.get_end());

                    ChoiceInner::SubTask(ChoiceSubTask { name, start, end })
                }
                ActingProcessModel::Resource(a) => {
                    let resource = var_id_as_cst(st, &a.resource);
                    let quantity = var_id_as_cst(st, &a.quantity);
                    let request = var_id_as_cst(st, &a.request);
                    let s_acq = var_id_as_cst(st, &a.acquisition.get_start());
                    let e_acq = var_id_as_cst(st, &a.acquisition.get_end());

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
    var: &VarId,
) -> Cst {
    match table
        .get_var(var)
        .unwrap_or_else(|| panic!("{var} has no binding in the planner"))
    {
        Variable::Bool(b) => {
            let lit = b.true_lit();
            let value = ass.value(lit).unwrap();
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
