use crate::select_mode::SelectMode;

#[derive(Debug, Default, Clone)]
pub struct OMPASOptions {
    select_mode: SelectMode,
}

impl OMPASOptions {
    pub fn new(select_mode: SelectMode) -> Self {
        Self { select_mode }
    }

    pub fn set_select_mode(&mut self, select_mode: SelectMode) {
        self.select_mode = select_mode
    }

    pub fn get_select_mode(&self) -> &SelectMode {
        &self.select_mode
    }
}
