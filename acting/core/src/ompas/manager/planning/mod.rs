use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::model::add_domain_symbols;
use crate::model::chronicle::acting_binding::ActingBinding;
use crate::model::chronicle::ChronicleKind;
use crate::model::process_ref::{Label, MethodId, ProcessRef};
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
use crate::ompas::manager::state::state_manager::StateManager;
use crate::ompas::scheme::exec::state::ModState;
use crate::planning::planner::encoding::domain::encode_ctx;
use crate::planning::planner::encoding::instance::{encode_init, generate_instances};
use crate::planning::planner::encoding::problem_generation::{
    convert_into_chronicle_instance, ActionParam, PAction,
};
use crate::planning::planner::encoding::{PlannerDomain, PlannerProblem};
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::planning::planner::solver::{run_planner, PMetric};
use crate::{
    ChronicleDebug, OMPAS_CHRONICLE_DEBUG_ON, OMPAS_DEBUG_CONTINUOUS_PLANNING,
    OMPAS_PLAN_OUTPUT_ON, TOKIO_CHANNEL_SIZE,
};
use aries::collections::seq::Seq;
use aries::model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries::model::lang::Variable;
use aries::model::Model;
use aries_planning::chronicles;
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{ChronicleOrigin, FiniteProblem, TaskId, VarLabel};
use futures::future::abortable;
use itertools::Itertools;
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use planner_manager_interface::PlannerManagerInterface;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::mem;
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
    rx_update: mpsc::Receiver<PlannerUpdate>,
}

pub struct PlannerManager {}

impl PlannerManager {
    pub async fn run(
        acting_manager: RefInnerActingManager,
        state_manager: StateManager,
        resource_manager: ResourceManager,
        clock_manager: ClockManager,
        domain: OMPASDomain,
        st: RefSymTable,
        env: LEnv,
        opt: Option<PMetric>,
    ) -> PlannerManagerInterface {
        let (tx_update, rx_update) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        let pmi = PlannerManagerInterface::new(tx_update);

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
        let mut updater: Option<mpsc::Sender<VarUpdate>> = None;
        let mut plan_receiver: Option<oneshot::Receiver<ActingTreeUpdate>> = None;

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
            let interrupter = mem::replace(interrupter, None);
            if let Some(interrupter) = interrupter {
                let _ = interrupter.send(true);
            }
        }

        let mut clock = clock_manager.subscribe_to_clock().await;

        'main: loop {
            tokio::select! {
                Some(pu) = wait_plan(&mut plan_receiver) => {
                    plan_receiver = None;
                    interrupter = None;
                    updater = None;
                    acting_manager.write().await.update_acting_tree(pu).await
                }
                _ = process.recv() => {
                    interrupt(&mut interrupter);
                    break 'main;
                }
                Ok(_) = clock.changed() => {

                        //println!("tick {}", index);
                        let tick = *clock.borrow();
                        let mut updates = vec![];
                        while let Ok(update) = rx_update.try_recv() {
                            updates.push(update)
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
                                    PlannerUpdate::StateUpdate(_s) => {
                                        writeln!(explanation, "- State Update").unwrap();
                                    }
                                    PlannerUpdate::Plan => {
                                        writeln!(explanation, "- Requested replanning of tree.").unwrap();
                                    }
                                }
                            }
                            explanation
                        };
                        if planning {
                            interrupt(&mut interrupter);
                        }
                        let mut state = state_manager.get_snapshot().await;
                        let resource_state = resource_manager.get_snapshot().await;
                        state.absorb(resource_state);
                        let ep = ExecutionProblem {
                            state,
                            chronicles: acting_manager.read().await.get_current_chronicles(),
                        };
                        let mut env = env.clone();
                        env.update_context(ModState::new_from_snapshot(ep.state.clone()));

                        let pp: PlannerProblem = populate_problem(&domain, &env, &st, ep).await.unwrap();
                        if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                            for (origin, chronicle) in pp
                                .instances
                                .iter()
                                .map(|i| (i.origin.clone(), i.am.chronicle.as_ref().unwrap()))
                            {
                                println!("{:?}:\n{}", origin, chronicle)
                            }
                        }

                        let (problem, table) = encode(&st, &pp).await.unwrap();
                        if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                                for instance in &problem.chronicles {
                                    Printer::print_chronicle(&instance.chronicle, &problem.context.model);
                                }
                            }

                        let PlannerInstance {
                            _updater: u,
                            interrupter: i,
                            plan_receiver: p,
                        } = PlannerInstance::new(pp, problem, table, opt,explanation );
                        updater = Some(u);
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
    _updater: mpsc::Sender<VarUpdate>,
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
        let (_updater, _updated) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (interrupter, interrupted) = oneshot::channel();
        let (plan_sender, plan_receiver) = oneshot::channel();

        let (planner, handle) = abortable(tokio::spawn(async move {
            let result = run_planner(problem, opt);
            //println!("{}", format_partial_plan(&pb, &x)?);

            if let Ok(Some(pr)) = result {
                //result::print_chronicles(&pr);
                let PlanResult { ass, fp } = pr;

                let choices = extract_choices(&table, &ass, &fp.model, &planner_problem);
                let new_ams = extract_new_acting_models(&table, &ass, &fp.model, planner_problem);

                if OMPAS_PLAN_OUTPUT_ON.get() {
                    println!("Plan found");
                    for choice in &choices {
                        println!("{}:{}", choice.process_ref, choice.choice_inner)
                    }
                }

                //We update the plan with new acting models and choices extracted from the instanciation of variables of the planner.
                Some(ActingTreeUpdate {
                    acting_models: new_ams,
                    choices,
                })
            } else {
                if OMPAS_PLAN_OUTPUT_ON.get() {
                    println!("No solution found for planner");
                }
                None
            }
        }));

        tokio::spawn(async move {
            tokio::select! {
                _ = interrupted => {
                    handle.abort();
                    if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
                        println!("Interrupted planning for: \n{}", explanation);
                    }
                }
                Ok(Ok(Some(pu))) = planner => {
                    if OMPAS_DEBUG_CONTINUOUS_PLANNING.get() {
                        println!("Successfully planned for:\n{}", explanation);
                    }
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

    pub async fn update(&self, updates: Vec<VarUpdate>) {
        for update in updates {
            let _ = self._updater.send(update).await;
        }
    }
}

/*p = rx_update.recv() => {




        }
        PlannerUpdate::UpdateState(_) => {
            todo!()
        }
        PlannerUpdate::Instanciation(_) => {
            todo!()
        }
    }
}else {
    break 'main;
}
}*/

/*pub async fn run_continuous_planning(config: ContinuousPlanningConfig) {
    let mut process =
        ProcessInterface::new(CONTINUOUS_PLANNING, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let mut table;
    let ContinuousPlanningConfig {
        mut problem_receiver,
        plan_sender,
        domain,
        st,
        env,
        opt,
    } = config;

    'main: loop {
        tokio::select! {
            _ = process.recv() => {
                break 'main;
            }
            p = problem_receiver.recv() => {
                if let Some(p) = p {
                    let p: PlannerUpdate = p;
                    match p {
                        PlannerUpdate::ExecutionProblem(ep) => {
                            let mut env = env.clone();
                            env.update_context(ModState::new_from_snapshot(ep.state.clone()));

                            let pp: PlannerProblem = populate_problem(&domain, &env, &st, ep).await.unwrap();
                            if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                                for (origin, chronicle) in pp.instances.iter().map(|i| (i.origin.clone(), i.am.chronicle.as_ref().unwrap())) {
                                    println!("{:?}:\n{}", origin, chronicle)
                                }
                            }

                            let (aries_problem, t_table): (chronicles::Problem, ActingVarRefTable) = encode(&st, &pp).await.unwrap();
                            table = t_table;
                            if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                                for instance in &aries_problem.chronicles {
                                    Printer::print_chronicle(&instance.chronicle, &aries_problem.context.model);
                                }
                            }

                            let result = run_planner(
                                aries_problem,
                                opt,
                            );
                            //println!("{}", format_partial_plan(&pb, &x)?);

                            if let Ok(Some(pr)) = result {

                                //result::print_chronicles(&pr);
                                let PlanResult {
                                    ass,
                                    fp,
                                } = pr;

                                let choices = extract_choices(&table, &ass, &fp.model, &pp);
                                let new_ams = extract_new_acting_models(&table, &ass, &fp.model, pp);


                                if OMPAS_PLAN_OUTPUT_ON.get() {
                                    println!("Plan found");
                                    for choice in &choices {
                                        println!("{}:{}", choice.process_ref, choice.choice_inner)
                                    }
                                }

                                //We update the plan with new acting models and choices extracted from the instanciation of variables of the planner.
                                let pu = PlanUpdate {
                                    acting_models: new_ams,
                                    choices,
                                };

                                if plan_sender.send(pu).await.is_err() {
                                    panic!("error sending plan update")
                                }

                            } else {
                                println!("No solution found for planner")
                            };
                        }
                        PlannerUpdate::UpdateState(_) => {
                            todo!()
                        }
                        PlannerUpdate::Instanciation(_) => {
                            todo!()
                        }
                    }
                }else {
                    break 'main;
                }
            }

        }
    }
}*/

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
        let chronicle = ci.am.chronicle.as_ref().unwrap();
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
            /*if label != "max-q" && label != "quantity" {
                println!("detected sf: {}", label);
            }*/
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
            //tasks.insert(task.get_label().to_string());

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
                        let method_id = MethodId {
                            refinement_id: 0,
                            method_number: i,
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
        } else {
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

    /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::Full {
        println!("sf_labels: {:?}\nstate functions {:?}", sf_labels, sf)
    }*/

    Ok(PlannerProblem {
        instances,
        templates: vec![],
        domain: PlannerDomain {
            sf,
            methods,
            tasks,
            commands,
        },
        state,
    })
}

/// Encode the chronicles in the aries format in order to then call the planner lcp.
pub async fn encode(
    st: &RefSymTable,
    pp: &PlannerProblem,
) -> anyhow::Result<(chronicles::Problem, ActingVarRefTable)> {
    let mut table = ActingVarRefTable::default();
    let domain = &pp.domain;
    let mut context = encode_ctx(st, domain, &pp.state.instance)?;
    let mut chronicles = generate_instances(&mut context, &mut table, &pp.instances)?;
    let mut present_sf: Vec<String> = pp
        .domain
        .sf
        .iter()
        .map(|sf| sf.get_label().to_string())
        .collect();

    present_sf.append(&mut vec![QUANTITY.to_string(), MAX_Q.to_string()]);

    encode_init(&context, &pp.state, &present_sf, &mut chronicles[0]);

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
    table: &ActingVarRefTable,
    ass: &Arc<SavedAssignment>,
    model: &Model<VarLabel>,
    mut pp: PlannerProblem,
) -> Vec<ChronicleInstance> {
    pp.instances
        .drain(..)
        .filter(|c| c.generated)
        /*.filter(|c| {
            let presence = c.am.chronicle.as_ref().unwrap().get_presence();
            let cst = get_var_as_cst(table, ass, model, presence);
            //println!("{cst}");
            Cst::Bool(true) == cst
        })*/
        .collect()
}

pub struct ActingPlanResult {
    pub instances: Vec<ChronicleInstance>,
    pub table: ActingVarRefTable,
    pub assignements: Arc<SavedAssignment>,
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
        let presence = c.am.chronicle.as_ref().unwrap().get_presence();
        let cst = get_var_as_cst(table, ass, model, presence);
        //println!("{cst}");
        Cst::Bool(true) == cst
    }) {
        let pr = instance.pr.clone();
        let chronicle = instance.am.chronicle.as_ref().unwrap();
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
                method_id: instance.method_id,
            },
        ));

        'choice: for (label, binding) in &chronicle.bindings.inner {
            let mut pr = pr.clone();
            pr.push(*label);
            let choice: ChoiceInner = match binding {
                ActingBinding::Arbitrary(a) => {
                    let val = var_id_as_cst(st, &a.var_id);

                    ChoiceInner::Arbitrary(ChoiceArbitrary { val })
                }
                ActingBinding::Action(action) => {
                    let name: Vec<Cst> = action
                        .name
                        .iter()
                        .map(|var_id| var_id_as_cst(st, var_id))
                        .collect();

                    let start = var_id_as_cst(st, &action.interval.get_start());

                    let end = var_id_as_cst(st, &action.interval.get_end());

                    ChoiceInner::SubTask(ChoiceSubTask { name, start, end })
                }
                ActingBinding::Acquire(a) => {
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
