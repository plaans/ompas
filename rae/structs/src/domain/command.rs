use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Command {
    pub label: String,
    pub parameters: Parameters,
    pub body: LValue,
    pub model: LValue,
}

/*
GETTERS
 */
impl Command {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_body(&self) -> &LValue {
        &self.exec
    }

    pub fn get_model(&self) -> &LValue {
        &self.sim
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }
}

/*
SETTERS
 */
impl Command {
    pub fn set_parameters(&mut self, params: Parameters) {
        self.parameters = params
    }

    pub fn set_exec(&mut self, exec: LValue) {
        self.exec = exec
    }

    pub fn set_sim(&mut self, sim: LValue) {
        self.sim = sim
    }

    pub fn set_label(&mut self, label: String) {
        self.label = label
    }
}

impl Command {
    pub fn new(label: impl Display, parameters: Parameters, body: LValue, model: LValue) -> Self {
        Self {
            label: label.to_string(),
            parameters,
            body,
            model,
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "label: {}, parameters : {}\n exec: {}\n sim: {} ",
            self.label,
            self.parameters,
            self.body.format("exec: ".len()),
            self.model.format("sim: ".len())
        )
    }
}
