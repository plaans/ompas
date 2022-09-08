use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Command {
    label: String,
    parameters: Parameters,
    body: LValue,
    model: LValue,
    cost: LValue,
}

impl Command {
    pub fn new(
        label: impl Display,
        parameters: Parameters,
        body: LValue,
        model: LValue,
        cost: LValue,
    ) -> Self {
        Self {
            label: label.to_string(),
            parameters,
            body,
            model,
            cost,
        }
    }

    /*
    GETTERS
     */

    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }

    pub fn get_model(&self) -> &LValue {
        &self.model
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }

    pub fn get_cost(&self) -> &LValue {
        &self.cost
    }

    /*
    SETTERS
     */

    pub fn set_parameters(&mut self, params: Parameters) {
        self.parameters = params
    }

    pub fn set_body(&mut self, body: LValue) {
        self.body = body
    }

    pub fn set_model(&mut self, model: LValue) {
        self.model = model
    }

    pub fn set_label(&mut self, label: String) {
        self.label = label
    }

    pub fn set_cost(&mut self, cost: LValue) {
        self.cost = cost
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "label: {}, parameters : {}\n exec: {}\n sim: {}\n cost: {} ",
            self.label,
            self.parameters,
            self.body.format("exec: ".len()),
            self.model.format("sim: ".len()),
            self.cost.format("cost: ".len()),
        )
    }
}
