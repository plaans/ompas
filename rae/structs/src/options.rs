use core::option::Option;
use core::option::Option::{None, Some};
use std::fmt::{Display, Formatter};

#[derive(Debug, Default, Clone)]
pub struct RAEOptions {
    select_mode: SelectMode,
    platform_config: Option<String>,
}

impl Display for SelectMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SelectMode::Greedy => GREEDY.to_string(),
                SelectMode::Planning(p, bool) => format!(
                    "{}{}",
                    p,
                    match bool {
                        true => " with optimisation",
                        false => " without optimisation",
                    }
                ),
                SelectMode::Heuristic => HEURISTIC.to_string(),
                SelectMode::Learning => LEARNING.to_string(),
            }
        )
    }
}

impl Default for SelectMode {
    fn default() -> Self {
        Self::Greedy
    }
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

pub const GREEDY: &str = "greedy";
pub const PLANNING: &str = "planning";
pub const HEURISTIC: &str = "heuristic";
pub const LEARNING: &str = "learning";

#[derive(Debug, Copy, Clone)]
pub enum SelectMode {
    Greedy,
    Planning(Planner, bool),
    Heuristic,
    Learning,
}

pub const ARIES: &str = "aries";
pub const ARIES_OPT: &str = "aries-opt";
pub const UPOM: &str = "upom";
pub const RAE_PLAN: &str = "rae-plan";

#[derive(Debug, Copy, Clone)]
pub enum Planner {
    Aries,
    UPOM,
    RAEPlan(RaePlanOption),
}

impl Display for Planner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Planner::Aries => ARIES.to_string(),
                Planner::UPOM => UPOM.to_string(),
                Planner::RAEPlan(options) =>
                    format!("{}:\ndr0: {}\nnr0: {}", RAE_PLAN, options.dr0, options.nro),
            }
        )
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct RaePlanOption {
    dr0: usize,
    nro: usize,
}

impl RaePlanOption {
    pub fn new(dr0: usize, nro: usize) -> Self {
        Self { dr0, nro }
    }

    pub fn set_dr0(&mut self, dr0: usize) {
        self.dr0 = dr0;
    }

    pub fn set_nr0(&mut self, nro: usize) {
        self.nro = nro;
    }

    pub fn get_dr0(&self) -> usize {
        self.dr0
    }

    pub fn get_nro(&self) -> usize {
        self.nro
    }
}
