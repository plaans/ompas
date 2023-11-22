use ompas_language::select::*;
use std::fmt::{Display, Formatter};

pub const UPOM_D_MAX_DEFAULT: u64 = 10;
pub const UPOM_N_RO_DEFAULT: u64 = 10;
pub const UPOM_TIMEOUT_DEFAULT: f64 = 1.0;
pub const UPOM_C_DEFAULT: f64 = 2.0;

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
    UPOM(UPOMConfig),
    CChoice(CChoiceConfig),
    RAEPlan(RAEPlanConfig),
}

#[derive(Copy, Clone, Debug)]
pub struct UPOMConfig {
    ///Number of methods to compare.
    d_max: u64,
    ///Number of simulation for commands.
    nro: u64,
    timeout: f64,
    mode: UPOMMode,
    c: f64,
    iterative_deepening: bool,
}

impl Default for UPOMConfig {
    fn default() -> Self {
        Self {
            d_max: UPOM_D_MAX_DEFAULT,
            nro: UPOM_N_RO_DEFAULT,
            timeout: UPOM_TIMEOUT_DEFAULT,
            mode: Default::default(),
            c: UPOM_C_DEFAULT,
            iterative_deepening: false,
        }
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub enum UPOMMode {
    #[default]
    Efficiency,
    Robustness,
}

impl UPOMConfig {
    pub fn get_d_max(&self) -> u64 {
        self.d_max
    }

    pub fn get_iterative_deepening(&self) -> bool {
        self.iterative_deepening
    }

    pub fn get_nro(&self) -> u64 {
        self.nro
    }

    pub fn get_timeout(&self) -> f64 {
        self.timeout
    }

    pub fn get_mode(&self) -> UPOMMode {
        self.mode
    }

    pub fn get_c(&self) -> f64 {
        self.c
    }
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
                Planner::UPOM(config) => format!("{}; config = {:?}", UPOM, config),
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
