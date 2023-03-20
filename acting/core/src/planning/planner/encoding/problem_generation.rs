use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::parameters::Parameters;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::process_ref::ProcessRef;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::planning::conversion::convert;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::{PConfig, PLEnv, PLValue};
use crate::planning::planner::problem::ChronicleInstance;
use aries_planning::chronicles::ChronicleOrigin;
use function_name::named;
use sompas_structs::lenv::LEnv;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalues::LValueS;

#[derive(Clone)]
pub enum ActionParam {
    Instantiated(LValueS),
    Uninstantiated(LValueS),
}

impl ActionParam {
    pub fn lvalues(&self) -> &LValueS {
        match self {
            Self::Instantiated(lv) => lv,
            Self::Uninstantiated(lv) => lv,
        }
    }
}

#[derive(Clone)]
pub struct PAction {
    //Partially instantiated action
    pub args: Vec<ActionParam>,
    //Encoding of the origin of the chronicle
    pub origin: ChronicleOrigin,
    //Process reference regarding the execution trace
    pub pr: ProcessRef,
}

/*
pub async fn finite_problem(
    mut goal_actions: Vec<PAction>,
    context: &ConversionContext,
) -> Result<PlanningProblem, LRuntimeError> {
    let st = context.st.clone();
    let high_level_actions: Vec<LValueS> = goal_actions
        .iter()
        .map(|action| LValueS::List(action.args.iter().map(|tp| tp.lvalues().clone()).collect()))
        .collect();

    let mut tasks: HashSet<String> = Default::default();
    let mut methods: HashSet<String> = Default::default();
    let mut commands: HashSet<String> = Default::default();
    let mut sf_labels: HashSet<String> = Default::default();
    let mut instances: Vec<ChronicleInstance> = vec![];

    let mut update_domain =
        |tasks: &mut Vec<PAction>, instance: &ChronicleInstance, instance_n: usize| {
            let chronicle = instance.am.chronicle.as_ref().unwrap();
            for (id, subtask) in chronicle.get_subtasks().iter().enumerate() {
                let mut value: Vec<ActionParam> = vec![];
                for e in &subtask.name {
                    let domain = st.get_domain_of_var(&e);

                    let val = match domain.as_cst() {
                        Some(cst) => ActionParam::Instantiated(cst.clone().into()),
                        None => ActionParam::Uninstantiated(st.format_variable(&e).into()),
                    };
                    value.push(val)
                }

                let mut pr = instance.pr.clone();
                pr.push(subtask.label.unwrap());

                tasks.insert(
                    0,
                    PAction {
                        args: value,
                        origin: ChronicleOrigin::Refinement {
                            instance_id: instance_n,
                            task_id: id,
                        },
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
        };

    while let Some(action) = goal_actions.pop() {
        let tps = &action.args;
        let instance_n = instances.len() + 1;
        if let Some(task) = context
            .domain
            .tasks
            .get(tps[0].lvalues().to_string().as_str())
        {
            tasks.insert(task.get_label().to_string());

            let params = task.get_parameters().get_labels();
            assert_eq!(params.len(), tps.len() - 1);

            match task.get_model() {
                Some(model) => {
                    let model_lambda: LLambda = model.try_into().expect("");

                    let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                        &model_lambda,
                        action,
                        None,
                        task.get_parameters(),
                        &context,
                        ChronicleKind::Task,
                    )
                    .await?;
                    instance.pr.push(Label::Refinement(None));
                    update_domain(&mut goal_actions, &instance, instance_n);

                    instances.push(instance)
                }
                None => {
                    for (_, m_label) in task.get_methods().iter().enumerate() {
                        methods.insert(m_label.to_string());
                        let method = context.domain.get_methods().get(m_label).unwrap();
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
                            env,
                            ChronicleKind::Method,
                        )
                        .await?;
                        instance.pr.push(Label::Refinement(None));
                        update_domain(&mut goal_actions, &instance, instance_n);

                        instances.push(instance);
                    }
                }
            }
        } else if let Some(command) = context
            .domain
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
                &context,
                ChronicleKind::Command,
            )
            .await?;
            update_domain(&mut goal_actions, &instance, instance_n);

            instances.push(instance)
        } else {
        }
    }

    let sf = context
        .domain
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

    Ok(PlanningProblem {
        domain: PlanningDomain {
            sf,
            methods: methods.drain().collect(),
            tasks: tasks.drain().collect(),
            commands: commands.drain().collect(),
            templates: vec![],
            st: st.clone(),
        },
        instance: PlanningInstance {
            state: context.state.clone(),
            tasks: high_level_actions,
            instances,
        },
        st,
    })
}*/

#[named]
pub async fn convert_into_chronicle_instance(
    lambda: &LLambda,
    p_action: PAction,
    task: Option<&[ActionParam]>,
    parameters: &Parameters,
    st: &RefSymTable,
    env: &LEnv,
    kind: ChronicleKind,
) -> Result<ChronicleInstance, LRuntimeError> {
    let mut pc = PConfig::default();

    let action = &p_action.args;

    let label = action[0].lvalues().to_string();
    let params = &action[1..];

    let symbol_id = st.get_sym_id(&label).unwrap();

    let mut ch = Chronicle::new(label.to_string(), kind, st.clone());
    let mut name: Vec<VarId> = vec![symbol_id];
    if let LambdaArgs::List(l) = lambda.get_params() {
        if l.len() != parameters.get_number() {
            return Err(lruntimeerror!(
                function_name!(),
                format!(
                    "for {}: definition of parameters are different({} != {})",
                    label,
                    lambda.get_params(),
                    parameters
                )
            ));
        }

        for (lambda_param, ((_, pt), task_param)) in
            l.iter().zip(parameters.inner().iter().zip(params))
        {
            let id = match task_param {
                ActionParam::Instantiated(lv) => {
                    pc.p_table
                        .add_instantiated(lambda_param.to_string(), lv.into());
                    match lv {
                        LValueS::Symbol(s) => st.new_symbol(s),
                        LValueS::Int(i) => st.new_int(*i),
                        LValueS::Float(f) => st.new_float(*f),
                        LValueS::Bool(b) => st.new_bool(*b),
                        _ => unreachable!(),
                    }
                }
                ActionParam::Uninstantiated(_) => {
                    let str = lambda_param.to_string();
                    pc.p_table
                        .add(str.to_string(), PLValue::unpure(str.to_string().into()));
                    let id = st.new_parameter(str, pt.get_domain().clone());
                    ch.add_var(id);
                    id
                }
            };
            name.push(id);
        }
    }
    ch.set_name(name.clone());
    ch.set_task(match task {
        Some(task) => {
            let mut task_name = name;
            task_name[0] = st.get_sym_id(&task[0].lvalues().to_string()).unwrap();
            task_name
        }
        None => name,
    });

    //Enforce presence of the chronicle
    /*let presence = *ch.get_presence();
    ch.replace(&presence, &st.new_bool(true));*/

    let lv = lambda.get_body();

    let mut p_env = PLEnv {
        env: env.clone(),
        unpure_bindings: Default::default(),
        pc: pc.clone(),
    };

    let om: ActingModel = convert(Some(ch), lv, &mut p_env, st.clone()).await?;

    Ok(ChronicleInstance {
        generated: true,
        origin: p_action.origin,
        am: om,
        pr: p_action.pr,
    })
}
