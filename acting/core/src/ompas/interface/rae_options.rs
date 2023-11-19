use crate::ompas::interface::select_mode::SelectMode;
use crate::OMPAS_PRE_COMPUTE_MODELS;

#[derive(Debug, Default, Clone)]
pub struct OMPASOptions {
    select_mode: SelectMode,
    pre_compute_models: bool,
}

impl OMPASOptions {
    pub fn new(select_mode: SelectMode) -> Self {
        Self {
            select_mode,
            pre_compute_models: OMPAS_PRE_COMPUTE_MODELS.get(),
        }
    }

    pub fn set_select_mode(&mut self, select_mode: SelectMode) {
        self.select_mode = select_mode
    }

    pub fn get_select_mode(&self) -> &SelectMode {
        &self.select_mode
    }

    pub fn set_pre_compute_models(&mut self, pre_compute_models: bool) {
        self.pre_compute_models = pre_compute_models
    }

    pub fn get_pre_compute_models(&self) -> bool {
        self.pre_compute_models
    }
}
