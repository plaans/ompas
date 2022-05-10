use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Task {
    pub body: LValue,
    pub label: String,
    pub parameters: Parameters,
    pub methods: Vec<String>,
}

impl Task {
    pub fn new(label: String, body: LValue, parameters: Parameters) -> Self {
        Self {
            body,
            label,
            parameters,
            methods: vec![],
        }
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }

    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_methods(&self) -> &Vec<String> {
        &self.methods
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }
}

impl Display for Task {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str_methods: String = '('.into();
        for (index, e) in self.methods.iter().enumerate() {
            str_methods.push_str(e.as_str());
            if index < self.methods.len() - 1 {
                str_methods.push(',');
            }
        }
        str_methods.push(')');

        write!(
            f,
            "-parameters: {}\n\
            -body: {}\n\
            -methods: {}\n",
            self.parameters,
            self.body.format("body: ".len()),
            str_methods
        )
    }
}
