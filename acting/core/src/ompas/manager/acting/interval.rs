use core::fmt::{Display, Formatter};
use core::option::Option;
use core::option::Option::{None, Some};
use num_integer::Integer;
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Sub};

pub type Instant = u128;

#[derive(Default, Debug, Copy, Clone)]
pub struct Timepoint {
    factor: u64,
    instant: Instant,
}

impl Timepoint {
    pub const MICROS_FACTOR: u64 = 1_000_000;
    pub const MILLIS_FACTOR: u64 = 1_000;

    pub fn as_secs(&self) -> f64 {
        self.instant as f64 / self.factor as f64
    }

    pub fn as_millis(&self) -> f64 {
        (self.instant * 1000) as f64 / self.factor as f64
    }

    pub fn new(instant: u128) -> Self {
        Self {
            factor: Self::MILLIS_FACTOR,
            instant,
        }
    }

    pub fn new_micros(instant: u128) -> Self {
        Self {
            factor: Self::MICROS_FACTOR,
            instant,
        }
    }

    pub fn new_with_factor(instant: u128, factor: u64) -> Self {
        Self { factor, instant }
    }
}

impl Sub for Timepoint {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        let lcm = self.factor.lcm(&rhs.factor);
        Duration::Finite(
            lcm,
            self.instant * ((lcm / self.factor) as u128)
                - rhs.instant * ((lcm / rhs.factor) as u128),
        )
    }
}

impl PartialEq for Timepoint {
    fn eq(&self, other: &Self) -> bool {
        let lcm = self.factor.lcm(&other.factor);

        (self.instant * (lcm / self.factor) as u128)
            == other.instant * ((lcm / other.factor) as u128)
    }
}

impl PartialOrd for Timepoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let lcm = self.factor.lcm(&other.factor);
        let a = self.instant * ((lcm / self.factor) as u128);
        let b = other.instant * ((lcm / other.factor) as u128);

        a.partial_cmp(&b)
    }
}

impl Display for Timepoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_secs())
    }
}

impl From<Instant> for Timepoint {
    fn from(value: Instant) -> Self {
        Self {
            factor: Self::MILLIS_FACTOR,
            instant: value,
        }
    }
}

impl From<f64> for Timepoint {
    fn from(value: f64) -> Self {
        Self {
            factor: Self::MILLIS_FACTOR,
            instant: (value as u64 * Self::MILLIS_FACTOR) as u128,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Interval {
    pub start: Timepoint,
    pub end: Option<Timepoint>,
}

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("[{:.3},", self.start.as_secs());
        match &self.end {
            Some(end) => str.push_str(format!("{:.3}]", end.as_secs()).as_str()),
            None => str.push_str("...]"),
        }

        write!(f, "{}", str)
    }
}

impl Interval {
    pub fn new(start: impl Into<Timepoint>, end: Option<impl Into<Timepoint>>) -> Self {
        Self {
            start: start.into(),
            end: end.map(|i| i.into()),
        }
    }

    pub fn new_instant(start: impl Into<Timepoint>) -> Self {
        Self {
            start: start.into(),
            end: None,
        }
    }

    /// Returns end - start if end is defined.
    pub fn duration(&self) -> Duration {
        match &self.end {
            Some(e) => *e - self.start,
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
    Finite(u64, u128),
    Inf,
}

impl Duration {
    pub fn as_millis(&self) -> f64 {
        match self {
            Self::Finite(f, u) => (*u * 1000) as f64 / *f as f64,
            Duration::Inf => f64::MAX,
        }
    }

    pub fn as_secs(&self) -> f64 {
        match self {
            Self::Finite(f, u) => *u as f64 / *f as f64,
            Duration::Inf => f64::MAX,
        }
    }

    pub fn is_finite(&self) -> bool {
        matches!(self, Self::Finite(..))
    }
}

impl Display for Duration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Finite(..) => write!(f, "{} s", self.as_secs()),
            Self::Inf => write!(f, "inf"),
        }
    }
}

impl Add for Duration {
    type Output = Duration;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite(f1, a), Self::Finite(f2, b)) => {
                let lcm = f1.lcm(&f2);
                Self::Finite(lcm, a * u128::from(lcm / f1) + b * u128::from(lcm / f2))
            }
            _ => Self::Inf,
        }
    }
}

impl Sub for Duration {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite(f1, a), Self::Finite(f2, b)) => {
                let lcm = f1.lcm(&f2);
                Self::Finite(lcm, a * u128::from(lcm / f1) - b * u128::from(lcm / f2))
            }
            _ => Self::Inf,
        }
    }
}

impl AddAssign for Duration {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
