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
        if n == 0 {
            Self::default()
        } else {
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
    EfficiencyScore,
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
    NumberTasks,
    NumberMethods,
    NumberCommands,
    NumberAcquisitions,
    NumberArbitraries,
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
            EfficiencyScore,
            BestScoreRatio,
            DistanceToBestScore,
            NumberRetries,
            NumberFailures,
            PlanningWaitingTime,
            PlanningWaitingTimeRatio,
            PlanningTime,
            PlanningTimeRatio,
            NumberPlanningInstance,
            AveragePlanningTime,
            PlanningSuccessRate,
            NumberTasks,
            NumberMethods,
            NumberCommands,
            NumberAcquisitions,
            NumberArbitraries,
        ]
    }
}

pub const BENCH_MIN_TIME: &str = "Bench min. time";
pub const BENCH_MAX_TIME: &str = "Bench max. time";
pub const EXECUTION_TIME: &str = "ET";
pub const DISTANCE_TO_BEST_EXECUTION_TIME: &str = "DBET";
pub const BEST_EXECUTION_TIME_RATIO: &str = "RBET";
pub const DELIBERATION_TIME: &str = "DT";
pub const DELIBERATION_TIME_RATIO: &str = "DTR";
pub const COVERAGE: &str = "SR";
pub const SCORE: &str = "ES";
pub const NUMBER_RETRIES: &str = "NR";
pub const NUMBER_FAILURES: &str = "NF";
pub const DISTANCE_TO_BEST_SCORE: &str = "DBES";
pub const BEST_SCORE_RATIO: &str = "RBES";
pub const PLANNING_TIME: &str = "PT";
pub const PLANNING_TIME_RATIO: &str = "PTR";
pub const NUMBER_PLANNING_INSTANCE: &str = "N_{PI}";
pub const AVERAGE_PLANNING_TIME: &str = "APT";
pub const PLANNING_SUCCESS_RATE: &str = "PST";
pub const PLANNING_WAITING_TIME: &str = "PWT";
pub const PLANNING_WAITING_TIME_RATIO: &str = "PWTR";
pub const PLANNING_SOLUTIONS: &str = "NPS";
pub const NUMBER_TASKS: &str = "NT";
pub const NUMBER_COMMANDS: &str = "NC";
pub const NUMBER_METHODS: &str = "NM";
pub const NUMBER_ACQUISITIONS: &str = "NAcq";
pub const NUMBER_ARBITRARIES: &str = "NArb";

impl Field {
    pub fn to_latex(&self) -> String {
        match self {
            BenchMinTime => "$Min(BT)$",
            BenchMaxTime => "$Max(BT)$",
            ExecutionTime => "$T_E$",
            BestExecutionTimeRatio => "$RT_E^{*}$",
            DistanceToBestExecutionTime => "$DT_E^{*}$",
            DeliberationTime => "$T_D$",
            DeliberationTimeRatio => "$RT_D$",
            Coverage => "\\textit{{Cov}}",
            EfficiencyScore => "$E_S$",
            BestScoreRatio => "$RE_S^{{*}}$",
            DistanceToBestScore => "$DE^{*}_S$",
            NumberRetries => "$N_R$",
            NumberFailures => "N_F",
            PlanningWaitingTime => "$T_{WP}$",
            PlanningWaitingTimeRatio => "$RT_{WP}$",
            PlanningTime => "$T_{P}$",
            PlanningTimeRatio => "$RT_{P}$",
            NumberPlanningInstance => "$N_{PI}$",
            AveragePlanningSolutions => "$E_{PS}$",
            AveragePlanningTime => "$ET_P$",
            PlanningSuccessRate => "$SR_{P}$",
            NumberTasks => "$N_T$",
            NumberMethods => "$N_M$",
            NumberCommands => "$N_C$",
            NumberAcquisitions => "N_{Acq}",
            NumberArbitraries => "N_{Arb}",
        }
        .to_string()
    }

    pub fn unit(&self) -> String {
        match self {
            BenchMinTime | BenchMaxTime | ExecutionTime | DeliberationTime
            | PlanningWaitingTime | PlanningTime | AveragePlanningTime => "seconds",
            BestExecutionTimeRatio
            | DistanceToBestExecutionTime
            | DeliberationTimeRatio
            | Coverage
            | BestScoreRatio
            | DistanceToBestScore
            | PlanningSuccessRate
            | PlanningWaitingTimeRatio
            | PlanningTimeRatio => "percentage",
            EfficiencyScore => "tasks/seconds",
            NumberRetries
            | NumberFailures
            | NumberPlanningInstance
            | AveragePlanningSolutions
            | NumberCommands
            | NumberTasks
            | NumberMethods
            | NumberAcquisitions
            | NumberArbitraries => "",
        }
        .to_string()
    }

    pub fn unit_short(&self) -> String {
        match self {
            BenchMinTime | BenchMaxTime | ExecutionTime | DeliberationTime
            | PlanningWaitingTime | PlanningTime | AveragePlanningTime => "s",
            BestExecutionTimeRatio
            | DistanceToBestExecutionTime
            | DeliberationTimeRatio
            | Coverage
            | BestScoreRatio
            | DistanceToBestScore
            | PlanningSuccessRate
            | PlanningWaitingTimeRatio
            | PlanningTimeRatio => "\\%",
            EfficiencyScore => "T/s",
            NumberRetries
            | NumberFailures
            | NumberPlanningInstance
            | AveragePlanningSolutions
            | NumberCommands
            | NumberTasks
            | NumberMethods
            | NumberAcquisitions
            | NumberArbitraries => "",
        }
        .to_string()
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
                EfficiencyScore => SCORE,
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
                NumberTasks => {
                    NUMBER_TASKS
                }
                NumberMethods => {
                    NUMBER_METHODS
                }
                NumberCommands => {
                    NUMBER_COMMANDS
                }
                NumberAcquisitions => {
                    NUMBER_ACQUISITIONS
                }
                NumberArbitraries => {
                    NUMBER_ARBITRARIES
                }
            }
        )
    }
}
