use crate::stat::Field::*;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Sub};

pub mod config;
pub mod instance;
pub mod planning;
pub mod problem;
pub mod system;

#[derive(Default, Copy, Clone)]
pub struct Stat {
    pub best: bool,
    pub mean: f64,
    pub sd: f64,
    pub se: f64,
}

impl Stat {
    pub fn mean(&self) -> f64 {
        self.mean
    }
    pub fn sd(&self) -> f64 {
        self.sd
    }

    pub fn se(&self) -> f64 {
        self.se
    }
}

impl Stat {
    pub fn new(t: impl Into<f64>) -> Self {
        Self {
            best: false,
            mean: t.into(),
            sd: 0.0,
            se: 0.0,
        }
    }

    pub fn abs(&self) -> Self {
        Self {
            best: false,
            mean: self.mean.abs(),
            sd: self.sd,
            se: self.se,
        }
    }
}

impl Mul for Stat {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            best: false,
            mean: self.mean * rhs.mean,
            sd: ((self.sd / self.mean).powi(2) + (rhs.sd / rhs.mean).powi(2)).sqrt()
                * self.mean
                * rhs.mean,
            se: ((self.se / self.mean).powi(2) + (rhs.se / rhs.mean).powi(2)).sqrt()
                * self.mean
                * rhs.mean,
        }
    }
}

impl Div for Stat {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            best: false,
            mean: self.mean / rhs.mean,
            sd: ((self.sd / self.mean).powi(2) + (rhs.sd / rhs.mean).powi(2)).sqrt() * self.mean
                / rhs.mean,
            se: ((self.se / self.mean).powi(2) + (rhs.se / rhs.mean).powi(2)).sqrt() * self.mean
                / rhs.mean,
        }
    }
}

impl Add for Stat {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            best: false,
            mean: self.mean + rhs.mean,
            sd: (self.sd.powi(2) + rhs.sd.powi(2)).sqrt(),
            se: (self.se.powi(2) + rhs.se.powi(2)).sqrt(),
        }
    }
}

impl Sub for Stat {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            best: false,
            mean: self.mean - rhs.mean,
            sd: (self.sd.powi(2) + rhs.sd.powi(2)).sqrt(),
            se: (self.se.powi(2) + rhs.se.powi(2)).sqrt(),
        }
    }
}

impl<T: Clone> From<&[T]> for Stat
where
    f64: From<T>,
{
    fn from(value: &[T]) -> Self {
        let n = value.len();
        let values: Vec<f64> = value.iter().map(|t| t.clone().into()).collect();
        let mean = values.iter().sum::<f64>() / n as f64;
        let standard_deviation =
            (values.iter().map(|f| (f - mean).powi(2)).sum::<f64>() / n as f64).sqrt();
        Self {
            best: false,
            mean,
            sd: standard_deviation,
            se: standard_deviation / (n as f64).sqrt(),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Field {
    BenchMinTime,
    BenchMaxTime,
    ExecutionTime,
    BestExecutionTimeRatio,
    DistanceToBestExecutionTime,
    DeliberationTime,
    DeliberationTimeRatio,
    Coverage,
    Score,
    BestScoreRatio,
    DistanceToBestScore,
    NumberRetries,
    NumberFailures,
    PlanningWaitingTime,
    PlanningWaitingTimeRatio,
    PlanningTime,
    PlanningTimeRatio,
    NumberPlanningInstance,
    AveragePlanningSolutions,
    AveragePlanningTime,
    PlanningSuccessRate,
}

impl Field {
    pub fn all() -> Vec<Field> {
        vec![
            BenchMinTime,
            BenchMaxTime,
            ExecutionTime,
            BestExecutionTimeRatio,
            DistanceToBestExecutionTime,
            DeliberationTime,
            DeliberationTimeRatio,
            Coverage,
            Score,
            BestScoreRatio,
            DistanceToBestScore,
            NumberRetries,
            NumberFailures,
            PlanningTime,
            PlanningTimeRatio,
            NumberPlanningInstance,
            AveragePlanningTime,
            PlanningSuccessRate,
        ]
    }
}

pub const BENCH_MIN_TIME: &str = "Bench min. time";
pub const BENCH_MAX_TIME: &str = "Bench max. time";
pub const EXECUTION_TIME: &str = "T";
pub const DISTANCE_TO_BEST_EXECUTION_TIME: &str = "D_{BT}";
pub const BEST_EXECUTION_TIME_RATIO: &str = "R_{BT}";
pub const DELIBERATION_TIME: &str = "T_D";
pub const DELIBERATION_TIME_RATIO: &str = "R_D";
pub const COVERAGE: &str = "SR";
pub const SCORE: &str = "ES";
pub const NUMBER_RETRIES: &str = "N_R";
pub const NUMBER_FAILURES: &str = "N_F";
pub const DISTANCE_TO_BEST_SCORE: &str = "D_{BES}";
pub const BEST_SCORE_RATIO: &str = "R_{BES}";
pub const PLANNING_TIME: &str = "T_P";
pub const PLANNING_TIME_RATIO: &str = "R_P";
pub const NUMBER_PLANNING_INSTANCE: &str = "N_{PI}";
pub const AVERAGE_PLANNING_TIME: &str = "EPT";
pub const PLANNING_SUCCESS_RATE: &str = "SRP";
pub const PLANNING_WAITING_TIME: &str = "TWP";
pub const PLANNING_WAITING_TIME_RATIO: &str = "RWPT";
pub const PLANNING_SOLUTIONS: &str = "N_{PS}";

impl Field {
    pub fn to_latex(&self) -> String {
        format!("${}$", self)
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ExecutionTime => EXECUTION_TIME,
                DistanceToBestExecutionTime => DISTANCE_TO_BEST_EXECUTION_TIME,
                DeliberationTime => DELIBERATION_TIME,
                DeliberationTimeRatio => DELIBERATION_TIME_RATIO,
                Coverage => COVERAGE,
                Score => SCORE,
                DistanceToBestScore => DISTANCE_TO_BEST_SCORE,
                NumberRetries => NUMBER_RETRIES,
                NumberFailures => NUMBER_FAILURES,
                PlanningTime => PLANNING_TIME,
                PlanningTimeRatio => PLANNING_TIME_RATIO,
                NumberPlanningInstance => NUMBER_PLANNING_INSTANCE,
                AveragePlanningTime => AVERAGE_PLANNING_TIME,
                PlanningSuccessRate => PLANNING_SUCCESS_RATE,
                BestExecutionTimeRatio => BEST_EXECUTION_TIME_RATIO,
                BestScoreRatio => BEST_SCORE_RATIO,
                BenchMinTime => BENCH_MAX_TIME,
                BenchMaxTime => BENCH_MAX_TIME,
                PlanningWaitingTime => PLANNING_WAITING_TIME,
                PlanningWaitingTimeRatio => PLANNING_WAITING_TIME_RATIO,
                AveragePlanningSolutions => PLANNING_SOLUTIONS,
            }
        )
    }
}
