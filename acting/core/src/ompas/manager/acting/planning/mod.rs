use crate::model::acting_domain::OMPASDomain;
use crate::model::chronicle::acting_binding::ActingBinding;
use crate::model::chronicle::ChronicleKind;
use crate::model::process_ref::{Label, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::acting_var::{ActingVarRef, AsCst};
use crate::ompas::manager::acting::planning::plan_update::{
    Choice, ChoiceAcquire, ChoiceArbitrary, ChoiceInner, ChoiceRefinement, ChoiceSubTask,
    PlanUpdate,
};
use crate::ompas::manager::acting::planning::problem_update::{ExecutionProblem, ProblemUpdate};
use crate::ompas::manager::resource::WaiterPriority;
use crate::ompas::scheme::exec::state::ModState;
use crate::planning::planner::encoding::domain::encode_ctx;
use crate::planning::planner::encoding::instance::{encode_init, generate_instances};
use crate::planning::planner::encoding::problem_generation::{
    convert_into_chronicle_instance, ActionParam, PAction,
};
use crate::planning::planner::encoding::{PlannerDomain, PlannerProblem};
use crate::planning::planner::problem::ChronicleInstance;
use crate::planning::planner::result::PlanResult;
use crate::planning::planner::solver::run_solver;
use crate::planning::planner::solver::PMetric;
use aries::model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries::model::lang::{Atom, Variable};
use aries::model::Model;
use aries_planning::chronicles;
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{ChronicleOrigin, FiniteProblem, VarLabel};
use itertools::Itertools;
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::LLambda;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod plan_update;
pub mod problem_update;

const OPT: Option<PMetric> = None;

#[derive(Default)]
pub struct ActingVarRefTable {
    inner: im::HashMap<ActingVarRef, Variable>,
    reverse: im::HashMap<Variable, ActingVarRef>,
}

impl ActingVarRefTable {
    pub fn clear(&mut self) {
        self.inner.clear();
        self.reverse.clear();
    }

    pub fn add_binding(&mut self, id: impl Into<ActingVarRef>, var: Variable) {
        let id = id.into();
        self.inner.insert(id, var);
        self.reverse.insert(var, id);
    }

    pub fn contains(&mut self, id: impl Into<ActingVarRef>) -> bool {
        let id = id.into();
        self.inner.contains_key(&id)
    }

    pub fn get_var(&self, id: impl Into<ActingVarRef>) -> Option<&Variable> {
        let id = id.into();
        self.inner.get(&id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&ActingVarRef> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for ActingVarRefTable {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, r#ref) in &self.reverse {
            str.push_str(
                format!(
                    "{:?} <- {}\n",
                    Atom::from(*var),
                    r#ref.var_id().format(st, sym_version)
                )
                .as_str(),
            )
        }
        str
    }
}

const CONTINUOUS_PLANNING: &str = "continuous_planning";

pub struct ContinuousPlanningConfig {
    pub problem_receiver: mpsc::Receiver<ProblemUpdate>,
    pub plan_sender: mpsc::Sender<PlanUpdate>,
    pub domain: OMPASDomain,
    pub st: RefSymTable,
    pub env: LEnv,
    pub opt: Option<PMetric>,
}

pub async fn run_continuous_planning(config: ContinuousPlanningConfig) {
    let mut process =
        ProcessInterface::new(CONTINUOUS_PLANNING, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let mut table = ActingVarRefTable::default();
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
                    let p: ProblemUpdate = p;
                    match p {
                        ProblemUpdate::ExecutionProblem(ep) => {
                            let mut env = env.clone();
                            env.update_context(ModState::new_from_snapshot(ep.state.clone()));
                            let pp: PlannerProblem = populate_problem(&domain, &env, &st, ep).await.unwrap();
                            for (origin, chronicle) in pp.instances.iter().map(|i| (i.origin, i.am.chronicle.as_ref().unwrap())) {
                                println!("{:?}:{}", origin, chronicle)
                            }
                            let aries_problem: chronicles::Problem = encode(&mut table, &st, &pp).await.unwrap();
                            for instance in &aries_problem.chronicles {
                                Printer::print_chronicle(&instance.chronicle, &aries_problem.context.model);
                            }

                            let result = run_solver(
                                aries_problem,
                                opt,
                            );
                            //println!("{}", format_partial_plan(&pb, &x)?);

                            if let Ok(Some(pr)) = result {
                                println!("Plan found");
                                //result::print_chronicles(&pr);
                                let PlanResult {
                                    ass,
                                    fp,
                                } = pr;

                                let choices = extract_choices(&table, &ass, &fp.model, &pp);
                                let new_ams = extract_new_acting_models(&table, &ass, &fp.model, pp);

                                for choice in &choices {
                                    println!("{}:{}", choice.process_ref, choice.choice_inner)
                                }

                                //We update the plan with new acting models and choices extracted from the instanciation of variables of the planner.
                                let pu = PlanUpdate {
                                    acting_models: new_ams,
                                    choices,
                                };

                                if let Err(_) = plan_sender.send(pu).await {
                                    panic!("error sending plan update")
                                }

                            } else {
                                println!("No solution found for planner")
                            };
                        }
                        ProblemUpdate::UpdateState(_) => {
                            todo!()
                        }
                        ProblemUpdate::Instanciation(_) => {
                            todo!()
                        }
                    }
                }else {
                    break 'main;
                }
            }

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

    let mut tasks: HashSet<String> = Default::default();
    let mut methods: HashSet<String> = Default::default();
    let mut commands: HashSet<String> = Default::default();
    let mut sf_labels: HashSet<String> = Default::default();

    let mut update_problem = |p_actions: &mut Vec<PAction>,
                              em: &Vec<ChronicleInstance>,
                              chronicles: &ChronicleInstance,
                              instance_id: usize| {
        let chronicle = chronicles.am.chronicle.as_ref().unwrap();
        for (task_id, subtask) in chronicle.get_subtasks().iter().enumerate() {
            // If we do not find at least one chronicle to refine the subtask,
            // then we add a new chronicle

            let origin = ChronicleOrigin::Refinement {
                instance_id,
                task_id,
            };

            if em.iter().find(|ci| ci.origin == origin).is_none() {
                let mut value: Vec<ActionParam> = vec![];
                for e in &subtask.name {
                    let domain = st.get_domain_of_var(&e);

                    let val = match domain.as_cst() {
                        Some(cst) => ActionParam::Instantiated(cst.clone().into()),
                        None => ActionParam::Uninstantiated(st.format_variable(&e).into()),
                    };
                    value.push(val)
                }

                let mut pr = chronicles.pr.clone();
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

            for effect in chronicle.get_effects() {
                sf_labels.insert(effect.sv[0].format(&st, true));
            }

            for condition in chronicle.get_conditions() {
                sf_labels.insert(condition.sv[0].format(&st, true));
            }
        }
    };

    let mut p_actions = vec![];
    for (i, instance) in chronicles.iter().enumerate() {
        update_problem(&mut p_actions, &chronicles, instance, i)
    }
    let mut instances: Vec<ChronicleInstance> = chronicles;

    while let Some(action) = p_actions.pop() {
        let tps = &action.args;
        let instance_id = instances.len();
        if let Some(task) = domain.tasks.get(tps[0].lvalues().to_string().as_str()) {
            tasks.insert(task.get_label().to_string());

            let params = task.get_parameters().get_labels();
            assert_eq!(params.len(), tps.len() - 1);

            match task.get_model() {
                Some(model) => {
                    let model_lambda: LLambda = model.try_into().expect("");

                    let instance: ChronicleInstance = convert_into_chronicle_instance(
                        &model_lambda,
                        action,
                        None,
                        task.get_parameters(),
                        &st,
                        &env,
                        ChronicleKind::Task,
                    )
                    .await?;
                    //instance.pr.push(Label::Refinement(None));
                    update_problem(&mut p_actions, &vec![], &instance, instance_id);

                    instances.push(instance)
                }
                None => {
                    for (_, m_label) in task.get_methods().iter().enumerate() {
                        methods.insert(m_label.to_string());
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
                            &st,
                            &env,
                            ChronicleKind::Method,
                        )
                        .await?;
                        instance.pr.push(Label::Refinement(None));
                        update_problem(&mut p_actions, &vec![], &instance, instance_id);

                        instances.push(instance);
                    }
                }
            }
        } else if let Some(command) = domain
            .commands
            .get(action.args[0].lvalues().to_string().as_str())
        {
            commands.insert(command.get_label().to_string());

            let model_lambda: LLambda = command.get_model().try_into().expect("");

            let instance: ChronicleInstance = convert_into_chronicle_instance(
                &model_lambda,
                action,
                None,
                command.get_parameters(),
                &st,
                &env,
                ChronicleKind::Command,
            )
            .await?;
            update_problem(&mut p_actions, &vec![], &instance, instance_id);

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

    Ok(PlannerProblem {
        instances,
        templates: vec![],
        domain: PlannerDomain {
            sf,
            methods: methods.drain().collect(),
            tasks: tasks.drain().collect(),
            commands: commands.drain().collect(),
        },
        state,
    })
}

/// Encode the chronicles in the aries format in order to then call the planner lcp.
pub async fn encode(
    table: &mut ActingVarRefTable,
    st: &RefSymTable,
    pp: &PlannerProblem,
) -> anyhow::Result<chronicles::Problem> {
    let domain = &pp.domain;
    let mut context = encode_ctx(st, domain, &pp.state.instance)?;
    let mut chronicles = generate_instances(&mut context, table, &pp.instances)?;
    let mut present_sf: Vec<String> = pp
        .domain
        .sf
        .iter()
        .map(|sf| sf.get_label().to_string())
        .collect();

    present_sf.append(&mut vec![QUANTITY.to_string(), MAX_Q.to_string()]);

    encode_init(&context, &pp.state, &present_sf, &mut chronicles[0]);

    Ok(chronicles::Problem {
        context,
        templates: vec![],
        chronicles,
    })
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
        .filter(|c| {
            let presence = c.am.chronicle.as_ref().unwrap().get_presence();
            let cst = get_var_as_cst(table, ass, model, presence);
            //println!("{cst}");
            Cst::Bool(true) == cst
        })
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

        choices.push(Choice::new(pr.clone(), ChoiceRefinement { start, end }));

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
