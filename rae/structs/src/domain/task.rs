use crate::domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Task {
    body: LValue,
    model: LValue,
    label: String,
    parameters: Parameters,
    methods: Vec<String>,
}

impl Task {
    pub fn new(label: String, parameters: Parameters, body: LValue, model: LValue) -> Self {
        Self {
            body,
            model,
            label,
            parameters,
            methods: vec![],
        }
    }

    /*
    GETTERS
     */
    pub fn get_body(&self) -> &LValue {
        &self.body
    }
    pub fn get_model(&self) -> &LValue {
        &self.model
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

    /*
    SETTERS
     */
    pub fn set_body(&mut self, body: LValue) {
        self.body = body
    }

    pub fn set_model(&mut self, model: LValue) {
        self.model = model
    }

    pub fn set_parameters(&mut self, parameters: Parameters) {
        self.parameters = parameters
    }

    pub fn add_method(&mut self, method_label: String) {
        self.methods.push(method_label)
    }

    pub fn set_label(&mut self, label: String) {
        self.label = label
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
