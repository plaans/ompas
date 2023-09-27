use ompas_language::select::*;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub enum SelectMode {
    Greedy,
    Random,
    Score,
    Planning(Planner),
    Heuristic,
    Learning,
}

#[derive(Debug, Copy, Clone)]
pub enum Planner {
    Aries(bool),
    UPOM,
    CChoice(CChoiceConfig),
    RAEPlan(RAEPlanConfig),
}

#[derive(Copy, Clone, Default, Debug)]
pub struct CChoiceConfig {
    ///Number of methods to compare.
    b: Option<usize>,
    ///Number of simulation for commands.
    k: usize,
    max_depth: Option<usize>,
}

impl CChoiceConfig {
    pub fn get_b(&self) -> Option<usize> {
        self.b
    }

    pub fn get_k(&self) -> usize {
        self.k
    }

    pub fn get_max_depth(&self) -> Option<usize> {
        self.max_depth
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct RAEPlanConfig {
    //Number of methods to compare.
    b: Option<usize>,
    //Number of simulation for commands.
    k: usize,
    max_depth: Option<usize>,
}

impl RAEPlanConfig {
    pub fn get_b(&self) -> Option<usize> {
        self.b
    }

    pub fn get_k(&self) -> usize {
        self.k
    }

    pub fn get_max_depth(&self) -> Option<usize> {
        self.max_depth
    }
}

impl Display for Planner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Planner::Aries(true) => ARIES_OPT.to_string(),
                Planner::Aries(false) => ARIES.to_string(),
                Planner::UPOM => UPOM.to_string(),
                Planner::RAEPlan(config) => format!("{} ; config = {:?}", RAE_PLAN, config),
                Planner::CChoice(config) => format!("{} ; config = {:?}", C_CHOICE, config),
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
                SelectMode::Planning(p) => format!("{}", p),
                SelectMode::Heuristic => HEURISTIC.to_string(),
                SelectMode::Learning => LEARNING.to_string(),
                SelectMode::Random => RANDOM.to_string(),
                SelectMode::Score => SCORE.to_string(),
            }
        )
    }
}

impl Default for SelectMode {
    fn default() -> Self {
        Self::Greedy
    }
}
