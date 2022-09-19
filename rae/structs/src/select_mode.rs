use ompas_rae_language::*;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub enum SelectMode {
    Greedy,
    Planning(Planner, bool),
    Heuristic,
    Learning,
}

#[derive(Debug, Copy, Clone)]
pub enum Planner {
    Aries,
    UPOM,
    CChoice,
    RAEPlan(RaePlanOption),
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
                Planner::CChoice => C_CHOICE.to_string(),
            }
        )
    }
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
