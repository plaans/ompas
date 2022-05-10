use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct StateFunction {
    pub parameters: Parameters,
    pub body: LValue,
}

impl StateFunction {
    pub fn new(parameters: Parameters, body: LValue) -> Self {
        Self { parameters, body }
    }
}

impl Display for StateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "parameters : {}\nbody: {}",
            self.parameters,
            self.body.format("exec: ".len()),
        )
    }
}

impl StateFunction {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }
}
