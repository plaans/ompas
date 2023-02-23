use crate::conversion::chronicle::convert_graph;
use crate::conversion::chronicle::post_processing::post_processing;
use crate::conversion::flow::annotate::annotate;
use crate::conversion::flow::convert_lv;
use crate::conversion::flow::p_eval::p_eval;
use crate::conversion::flow::p_eval::r#struct::{PConfig, PLEnv, PLValue};
use crate::conversion::flow::post_processing::flow_graph_post_processing;
use crate::conversion::flow::pre_processing::pre_processing;
use aries_planning::chronicles::ChronicleOrigin;
use function_name::named;
use ompas_structs::acting_domain::parameters::Parameters;
use ompas_structs::acting_domain::task::Task;
use ompas_structs::conversion::chronicle::template::{ChronicleKind, ChronicleTemplate};
use ompas_structs::conversion::context::ConversionContext;
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::planning::domain::PlanningDomain;
use ompas_structs::planning::instance::{ChronicleInstance, PlanningInstance};
use ompas_structs::planning::problem::PlanningProblem;
use ompas_structs::supervisor::process::process_ref::{Label, ProcessRef};
use ompas_structs::sym_table::r#trait::FormatWithSymTable;
use ompas_structs::sym_table::VarId;
use sompas_structs::llambda::{LLambda, LambdaArgs};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::HashSet;
use std::fmt::Display;
use std::time::SystemTime;

pub enum TaskParam {
    Instantiated(LValueS),
    Uninstantiated(LValueS),
}

impl TaskParam {
    pub fn lvalues(&self) -> &LValueS {
        match self {
            Self::Instantiated(lv) => lv,
            Self::Uninstantiated(lv) => lv,
        }
    }
}

pub struct PAction {
    pub args: Vec<TaskParam>,
    pub origin: ChronicleOrigin,
    pub pr: ProcessRef,
}

pub async fn finite_problem(
    mut actions: Vec<PAction>,
    context: &ConversionContext,
) -> Result<PlanningProblem, LRuntimeError> {
    let st = context.st.clone();
    let high_level_actions: Vec<LValueS> = actions
        .iter()
        .map(|action| LValueS::List(action.args.iter().map(|tp| tp.lvalues().clone()).collect()))
        .collect();

    let mut tasks: HashSet<String> = Default::default();
    let mut methods: HashSet<String> = Default::default();
    let mut commands: HashSet<String> = Default::default();
    let mut sf_labels: HashSet<String> = Default::default();
    let mut instances: Vec<ChronicleInstance> = vec![];

    let mut update_domain =
        |tasks: &mut Vec<PAction>, template: &ChronicleTemplate, instance_n: usize| {
            for (id, subtask) in template.get_subtasks().iter().enumerate() {
                let mut value: Vec<TaskParam> = vec![];
                for e in &subtask.task {
                    let domain = st.get_domain_of_var(&e);

                    let val = match domain.as_constant() {
                        Some(cst) => TaskParam::Instantiated(cst.clone().into()),
                        None => TaskParam::Uninstantiated(st.format_variable(&e).into()),
                    };
                    value.push(val)
                }

                tasks.push(PAction {
                    args: value,
                    origin: ChronicleOrigin::Refinement {
                        instance_id: instance_n,
                        task_id: id,
                    },
                    pr: Default::default(),
                })
            }

            for effect in template.get_effects() {
                sf_labels.insert(effect.sv[0].format(&st, true));
            }

            for condition in template.get_conditions() {
                sf_labels.insert(condition.sv[0].format(&st, true));
            }
        };

    while let Some(action) = actions.pop() {
        let tps = &action.args;
        let instance_n = instances.len() + 1;
        if let Some(task) = context
            .domain
            .tasks
            .get(tps[0].lvalues().to_string().as_str())
        {
            tasks.insert(task.get_label().to_string());

            let params = task.get_parameters().get_labels();
            let mut pc = PConfig::default();
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
                    update_domain(&mut actions, &instance.template, instance_n);

                    instances.push(instance)
                }
                None => {
                    todo!();
                    /*for (id, m_label) in task.get_methods().iter().enumerate() {
                        methods.insert(m_label.to_string());
                        let mut pc = pc.clone();
                        let method = context.domain.methods.get(m_label).unwrap();
                        for param in &method.parameters.get_labels()[params.len()..] {
                            pc.p_table.add_param(param.to_string());
                        }

                        let method_lambda: LLambda = method.get_body().try_into().expect("");

                        let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                            &method_lambda,
                            &method.label,
                            Some(task),
                            method.get_parameters(),
                            &context,
                            ChronicleKind::Method,
                            pc.clone(),
                        )
                        .await?;
                        update_domain(&mut actions, &instance.template, instance_n);
                        instance.pr = action.pr.clone();
                        instance.pr.push(Label::Method(id));
                        instances.push(instance);
                    }*/
                }
            }
        } else if let Some(command) = context
            .domain
            .commands
            .get(action.args[0].lvalues().to_string().as_str())
        {
            commands.insert(command.get_label().to_string());

            let model_lambda: LLambda = command.get_model().try_into().expect("");

            let mut instance: ChronicleInstance = convert_into_chronicle_instance(
                &model_lambda,
                action,
                None,
                command.get_parameters(),
                &context,
                ChronicleKind::Command,
            )
            .await?;
            update_domain(&mut actions, &instance.template, instance_n);

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
}

#[named]
pub async fn convert_into_chronicle_instance(
    lambda: &LLambda,
    p_action: PAction,
    task: Option<&[TaskParam]>,
    parameters: &Parameters,
    cc: &ConversionContext,
    kind: ChronicleKind,
) -> Result<ChronicleInstance, LRuntimeError> {
    let mut pc = PConfig::default();

    let action = &p_action.args;

    let time = SystemTime::now();

    let st = cc.st.clone();

    let label = action[0].lvalues().to_string();
    let params = &action[1..];

    let symbol_id = st.get_sym_id(&label).unwrap();

    let mut ch = ChronicleTemplate::new(label.to_string(), kind, st.clone());
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
                TaskParam::Instantiated(lv) => {
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
                TaskParam::Uninstantiated(lv) => {
                    let symbol = lv.to_string();
                    pc.p_table.add(
                        lambda_param.to_string(),
                        PLValue::unpure(symbol.as_str().into()),
                    );
                    let id = st.new_parameter(symbol, pt.get_domain().clone());
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

    let lv = lambda.get_body();
    ch.debug.lvalue = lv.clone();

    let mut p_env = PLEnv {
        env: cc.env.clone(),
        unpure_bindings: Default::default(),
        pc: pc.clone(),
    };

    let lv = p_eval(lv, &mut p_env).await?;
    let lv_om = annotate(lv);
    println!("{}", lv_om.format(0));
    //println!("lv: {}", lv.format(4));
    //panic!();
    let lv = pre_processing(&lv_om, &cc.env).await?;

    let mut graph = FlowGraph::new(st);

    let flow = convert_lv(&lv, &mut graph, &mut Default::default())?;
    graph.flow = flow;
    flow_graph_post_processing(&mut graph)?;
    let mut ch = convert_graph(Some(ch), &mut graph, &flow, &cc.env)?;
    post_processing(&mut ch, cc.env.clone())?;

    graph.flat_bindings();
    ch.debug.flow_graph = graph;
    ch.debug.post_processed_lvalue = lv;
    ch.debug.convert_time = time.elapsed().unwrap();

    Ok(ChronicleInstance {
        origin: p_action.origin,
        template: ch,
        value: lv_om,
        pr: p_action.pr,
    })
}
