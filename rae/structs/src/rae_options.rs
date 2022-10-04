use crate::select_mode::SelectMode;
use core::option::Option;
use core::option::Option::{None, Some};

#[derive(Debug, Default, Clone)]
pub struct RAEOptions {
    select_mode: SelectMode,
    platform_config: Option<String>,
}

impl RAEOptions {
    pub fn new(select_mode: SelectMode) -> Self {
        Self {
            select_mode,
            platform_config: None,
        }
    }

    pub fn new_with_platform_config(
        select_mode: SelectMode,
        platform_config: Option<String>,
    ) -> Self {
        Self {
            select_mode,
            platform_config,
        }
    }

    pub fn set_select_mode(&mut self, select_mode: SelectMode) {
        self.select_mode = select_mode
    }

    pub fn get_select_mode(&self) -> &SelectMode {
        &self.select_mode
    }

    pub fn set_platform_config(&mut self, str: String) {
        self.platform_config = Some(str);
    }

    pub fn get_platform_config(&self) -> Option<String> {
        self.platform_config.clone()
    }
}
