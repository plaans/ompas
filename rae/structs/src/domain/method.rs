use crate::domain::parameters::Parameters;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::llambda::LLambda;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Method {
    pub label: String,
    pub task_label: String,
    pub parameters: Parameters,
    pub lambda_pre_conditions: LValue,
    pub lambda_score: LValue,
    pub lambda_body: LValue,
}

//Getters
impl Method {
    pub fn get_task_label(&self) -> &String {
        &self.task_label
    }

    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_pre_conditions(&self) -> &LValue {
        &self.lambda_pre_conditions
    }

    pub fn get_body(&self) -> &LValue {
        &self.lambda_body
    }

    pub fn get_lambda(&self) -> LValue {
        let l1: LLambda = self.lambda_pre_conditions.borrow().try_into().expect("");
        let l2: LLambda = self.lambda_body.borrow().try_into().expect("");
        let body: LValue = vec![
            LCoreOperator::Do.into(),
            l1.get_body().clone(),
            l2.get_body().clone(),
        ]
        .into();
        let mut env = l1.get_env_symbols();
        env.set_outer(l2.get_env_symbols());
        LLambda::new(l1.get_params(), body, env).into()
    }
}

impl Method {
    pub fn new(
        label: String,
        task_label: String,
        parameters: Parameters,
        conds: LValue,
        score: LValue,
        body: LValue,
    ) -> Self {
        Self {
            label,
            task_label,
            parameters,
            lambda_pre_conditions: conds,
            lambda_score: score,
            lambda_body: body,
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "-task: {}\n\
            -parameters: {}\n\
            -pre-conditions: {}\n\
            -score: {}\n\
            -body: {}\n",
            self.task_label,
            self.parameters,
            self.lambda_pre_conditions.format("pre-conditions: ".len()),
            self.lambda_score.format("score: ".len()),
            self.lambda_body.format("body: ".len())
        )
    }
}
