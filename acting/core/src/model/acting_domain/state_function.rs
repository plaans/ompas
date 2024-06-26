use crate::model::acting_domain::parameters::Parameters;
use crate::model::sym_domain::Domain;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct StateFunction {
    label: String,
    pub parameters: Parameters,
    pub result: Domain,
    pub result_debug: String,
    pub body: Option<LValue>,
}

impl StateFunction {
    pub fn new(
        label: String,
        parameters: Parameters,
        result: Domain,
        result_debug: String,
        body: Option<LValue>,
    ) -> Self {
        Self {
            label,
            parameters,
            result,
            result_debug,
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
            self.result_debug,
            match &self.body {
                Some(body) => body.format("body: ".len()),
                None => "".to_string(),
            },
        )
    }
}

impl StateFunction {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_body(&self) -> &Option<LValue> {
        &self.body
    }

    pub fn get_label(&self) -> &str {
        &self.label
    }
}
