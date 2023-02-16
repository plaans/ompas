use core::fmt::{Display, Formatter};
use core::option::Option;
use core::option::Option::{None, Some};
use std::ops::{Add, AddAssign};

pub type Timepoint = u128;

#[derive(Debug, Default, Clone)]
pub struct Interval {
    pub start: Timepoint,
    pub end: Option<Timepoint>,
}

const FACTOR_TO_SEC: f64 = 1_000_000.0;
#[allow(dead_code)]
const FACTOR_TO_MILLIS: f64 = 1_000.0;

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("[{:.3},", self.start as f64 / FACTOR_TO_SEC);
        match &self.end {
            Some(end) => str.push_str(format!("{:^3}]", *end as f64 / FACTOR_TO_SEC).as_str()),
            None => str.push_str("...]"),
        }

        write!(f, "{}", str)
    }
}

impl Interval {
    pub fn new(start: Timepoint, end: Option<Timepoint>) -> Self {
        Self { start, end }
    }

    pub fn new_instant(start: Timepoint) -> Self {
        Self { start, end: None }
    }

    /// Returns end - start if end is defined.
    pub fn duration(&self) -> Duration {
        match &self.end {
            Some(e) => Duration::Finite(e - self.start),
            None => Duration::Inf,
        }
    }

    pub fn set_end(&mut self, end: Timepoint) {
        assert!(self.start <= end);
        self.end = Some(end)
    }
}
#[derive(Copy, Clone)]
pub enum Duration {
    Finite(u128),
    Inf,
}

impl Duration {
    pub fn as_millis(&self) -> f64 {
        match self {
            Self::Finite(u) => *u as f64 / FACTOR_TO_MILLIS,
            Duration::Inf => f64::MAX / FACTOR_TO_MILLIS,
        }
    }

    pub fn as_secs(&self) -> f64 {
        match self {
            Self::Finite(u) => *u as f64 / FACTOR_TO_SEC,
            Duration::Inf => f64::MAX / FACTOR_TO_SEC,
        }
    }

    pub fn is_finite(&self) -> bool {
        matches!(self, Self::Finite(_))
    }
}

impl Display for Duration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Finite(u) => write!(f, "{} µs", u),
            Self::Inf => write!(f, "inf"),
        }
    }
}

impl Add for Duration {
    type Output = Duration;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite(a), Self::Finite(b)) => Self::Finite(a + b),
            _ => Self::Inf,
        }
    }
}

impl AddAssign for Duration {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
