use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Action {
    pub label: String,
    pub parameters: Parameters,
    pub exec: LValue,
    pub sim: LValue,
}

impl Action {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_exec(&self) -> &LValue {
        &self.exec
    }

    pub fn get_sim(&self) -> &LValue {
        &self.sim
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }
}

impl Action {
    pub fn new(label: impl Display, parameters: Parameters, exec: LValue, sim: LValue) -> Self {
        Self {
            label: label.to_string(),
            parameters,
            exec,
            sim,
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "label: {}, parameters : {}\n exec: {}\n sim: {} ",
            self.label,
            self.parameters,
            self.exec.format("exec: ".len()),
            self.sim.format("sim: ".len())
        )
    }
}
