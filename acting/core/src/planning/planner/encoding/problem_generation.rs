use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::parameters::Parameters;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::process_ref::{MethodLabel, ProcessRef, RefinementLabel};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::planning::conversion::chronicle::post_processing::try_eval_apply;
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
            let mut task_name: Vec<VarId> = name[0..task.len()].to_vec();
            task_name[0] = st.get_sym_id(&task[0].lvalues().to_string()).unwrap();
            task_name
        }
        None => name,
    });

    let lv = lambda.get_body();

    let p_env = PLEnv {
        env: env.clone(),
        unpure_bindings: Default::default(),
        pc: pc.clone(),
    };

    let om: ActingModel = convert(Some(ch), lv, p_env, st.clone()).await?;

    let mut instantiated_chronicle = om
        .chronicle
        .as_ref()
        .unwrap()
        .clone()
        .instantiate_and_clean(Default::default());

    try_eval_apply(&mut instantiated_chronicle, &env).await?;

    Ok(ChronicleInstance {
        instantiated_chronicle,
        generated: true,
        origin: p_action.origin,
        am: om,
        pr: p_action.pr,
        refinement_label: RefinementLabel {
            refinement_id: 0,
            method_label: MethodLabel::Possibility(0),
        },
    })
}
