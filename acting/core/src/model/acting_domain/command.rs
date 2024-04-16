use crate::model::acting_domain::model::{ModelCollection, ModelKind};
use crate::model::acting_domain::parameters::Parameters;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Command {
    label: String,
    parameters: Parameters,
    body: LValue,
    pub model_collection: ModelCollection,
}

impl Command {
    pub fn new(
        label: impl Display,
        parameters: Parameters,
        body: LValue,
        model_collection: ModelCollection,
    ) -> Self {
        Self {
            label: label.to_string(),
            parameters,
            body,
            model_collection,
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

    pub fn get_model(&self, kind: &ModelKind) -> Option<LValue> {
        self.model_collection.get(kind)
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }

    pub fn get_cost(&self) -> Option<LValue> {
        self.model_collection.get(&ModelKind::CostModel)
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

    pub fn set_model(&mut self, model: LValue, kind: ModelKind) {
        self.model_collection.insert(model, kind);
    }

    pub fn set_label(&mut self, label: String) {
        self.label = label
    }

    pub fn set_cost(&mut self, cost: LValue) {
        self.model_collection.insert(cost, ModelKind::CostModel)
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "label: {}, parameters : {}\n exec: {}\n models: {}",
            self.label,
            self.parameters,
            self.body.format("exec: ".len()),
            self.model_collection,
        )
    }
}
