use crate::domain::_type::Type;
use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct StateFunction {
    label: String,
    pub parameters: Parameters,
    pub result: Type,
    pub body: LValue,
}

impl StateFunction {
    pub fn new(label: String, parameters: Parameters, result: Type, body: LValue) -> Self {
        Self {
            label,
            parameters,
            result,
            body,
        }
    }
}

impl Display for StateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "parameter(s) : {}\nresult: {}\nbody: {}",
            self.parameters,
            self.result,
            self.body.format("body: ".len()),
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

    pub fn get_label(&self) -> &str {
        &self.label
    }
}
