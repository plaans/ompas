use crate::model::acting_domain::model::{ModelCollection, ModelKind};
use crate::model::acting_domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Task {
    label: String,
    parameters: Parameters,
    methods: Vec<String>,
    body: LValue,
    models: ModelCollection,
}

impl Task {
    pub fn new(
        label: String,
        parameters: Parameters,
        body: LValue,
        models: ModelCollection,
    ) -> Self {
        Self {
            body,
            models,
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
    pub fn get_model(&self, kind: &ModelKind) -> Option<LValue> {
        self.models.get(kind)
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

    pub fn set_model(&mut self, model: LValue, kind: ModelKind) {
        self.models.insert(model, kind)
    }

    pub fn set_parameters(&mut self, parameters: Parameters) {
        self.parameters = parameters
    }

    pub fn add_method(&mut self, method_label: &str) {
        self.methods.push(method_label.to_string())
    }

    pub fn remove_method(&mut self, method_label: &str) {
        self.methods.retain(|m| m != method_label);
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
            "- parameters: {}\n 
            - body: {}\n\
            - methods: {}\n\
            - model: {}\n",
            self.parameters,
            self.body.format("- body: ".len()),
            str_methods,
            self.models,
            /*match &self.model {
                Some(model) => model.format("- model: ".len()),
                None => "none".to_string(),
            }*/
        )
    }
}
