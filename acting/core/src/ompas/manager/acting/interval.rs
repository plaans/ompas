use core::fmt::{Display, Formatter};
use core::option::Option;
use core::option::Option::{None, Some};
use num_integer::Integer;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::ops::{Add, AddAssign, Sub, SubAssign};

pub type Instant = u128;

#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct Timepoint {
    denom: u64,
    instant: Instant,
}

impl Timepoint {
    pub const MICROS_FACTOR: u64 = 1_000_000;
    pub const MILLIS_FACTOR: u64 = 1_000;

    fn normalize(t1: &Self, t2: &Self) -> (Self, Self) {
        let d = t1.denom.lcm(&t2.denom);
        (
            Self {
                denom: d,
                instant: t1.instant * ((d / t1.denom) as u128),
            },
            Self {
                denom: d,
                instant: t2.instant * ((d / t2.denom) as u128),
            },
        )
    }

    pub fn as_secs(&self) -> f64 {
        self.instant as f64 / self.denom as f64
    }

    pub fn as_millis(&self) -> f64 {
        (self.instant * 1000) as f64 / self.denom as f64
    }

    pub fn new_millis(instant: u128) -> Self {
        Self {
            denom: Self::MILLIS_FACTOR,
            instant,
        }
    }

    pub fn new_micros(instant: u128) -> Self {
        Self {
            denom: Self::MICROS_FACTOR,
            instant,
        }
    }

    pub fn new_with_factor(instant: u128, factor: u64) -> Self {
        Self {
            denom: factor,
            instant,
        }
    }
}

impl Sub for Timepoint {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        let denom = self.denom.lcm(&rhs.denom);
        Duration::Finite {
            num: self.instant * ((denom / self.denom) as u128)
                - rhs.instant * ((denom / rhs.denom) as u128),
            denom,
        }
    }
}

impl PartialEq for Timepoint {
    fn eq(&self, other: &Self) -> bool {
        let (t1, t2) = Self::normalize(self, other);
        t1.instant == t2.instant
    }
}

impl Eq for Timepoint {}

impl PartialOrd for Timepoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Timepoint {
    fn cmp(&self, other: &Self) -> Ordering {
        let (t1, t2) = Self::normalize(self, other);
        t1.instant.cmp(&t2.instant)
    }

    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (t1, t2) = Self::normalize(&self, &other);
        if t1.instant > t2.instant {
            self
        } else {
            other
        }
    }

    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (t1, t2) = Self::normalize(&self, &other);
        if t1.instant < t2.instant {
            self
        } else {
            other
        }
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
            denom: Self::MILLIS_FACTOR,
            instant: value,
        }
    }
}

impl From<f64> for Timepoint {
    fn from(value: f64) -> Self {
        Self {
            denom: Self::MILLIS_FACTOR,
            instant: (value * Self::MILLIS_FACTOR as f64) as u128,
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
#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Duration {
    Finite {
        num: u128,
        denom: u64,
    },
    #[default]
    Inf,
}

impl Eq for Duration {}

impl PartialEq<Self> for Duration {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Duration::Inf, Duration::Inf) => true,
            (Duration::Finite { num: n1, denom: d1 }, Duration::Finite { num: n2, denom: d2 }) => {
                let d = d1.lcm(d2);
                n1 * ((d / d1) as u128) == n2 * ((d / d2) as u128)
            }
            _ => false,
        }
    }
}

impl PartialOrd<Self> for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Duration {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Inf, Self::Inf) => Ordering::Equal,
            (Self::Inf, Self::Finite { .. }) => Ordering::Greater,
            (Self::Finite { .. }, Self::Inf) => Ordering::Less,
            (d1, d2) => {
                let (Self::Finite { num: n1, .. }, Self::Finite { num: n2, .. }) =
                    Self::normalize(d1, d2)
                else {
                    unreachable!()
                };

                n1.cmp(&n2)
            }
        }
    }
    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (d1, d2) = Self::normalize(&self, &other);
        if d1 > d2 {
            self
        } else {
            other
        }
    }
    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        let (d1, d2) = Self::normalize(&self, &other);
        if d1 < d2 {
            self
        } else {
            other
        }
    }
}

impl Duration {
    pub fn normalize(d1: &Self, d2: &Self) -> (Self, Self) {
        match (d1, d2) {
            (Self::Finite { num: n1, denom: d1 }, Self::Finite { num: n2, denom: d2 }) => {
                let denom = d1.lcm(d2);
                (
                    Self::Finite {
                        num: n1 * ((denom / d1) as u128),
                        denom,
                    },
                    Self::Finite {
                        num: n2 * ((denom / d2) as u128),
                        denom,
                    },
                )
            }
            _ => (*d1, *d2),
        }
    }

    pub fn zero() -> Self {
        Self::Finite { num: 0, denom: 1 }
    }

    pub fn as_millis(&self) -> f64 {
        match self {
            Self::Finite { num, denom } => (*num * 1000) as f64 / *denom as f64,
            Duration::Inf => f64::MAX,
        }
    }

    pub fn as_secs(&self) -> f64 {
        match self {
            Self::Finite { num, denom } => *num as f64 / *denom as f64,
            Duration::Inf => f64::MAX,
        }
    }

    pub fn is_finite(&self) -> bool {
        matches!(self, Self::Finite { .. })
    }

    pub fn min_of(durations: &[Duration]) -> Duration {
        let mut min = Self::Inf;
        for d in durations {
            if d < &min {
                min = *d
            }
        }
        min
    }

    pub fn max_of(durations: &[Duration]) -> Duration {
        let mut max = Self::zero();
        for d in durations {
            if d > &max {
                max = *d
            }
        }
        max
    }

    pub fn mean_of(durations: &[Duration]) -> Duration {
        let mut duration = Self::total_of(durations);
        if let Duration::Finite { num: _, denom } = &mut duration {
            *denom *= durations.len() as u64
        }
        duration
    }

    pub fn total_of(durations: &[Duration]) -> Duration {
        durations.iter().fold(Duration::zero(), |sum, d2| sum + *d2)
    }
}

impl Display for Duration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Finite { .. } => write!(f, "{} s", self.as_secs()),
            Self::Inf => write!(f, "inf"),
        }
    }
}

impl Add for Duration {
    type Output = Duration;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite { num: a, denom: f1 }, Self::Finite { num: b, denom: f2 }) => {
                let denom = f1.lcm(&f2);
                Self::Finite {
                    num: a * u128::from(denom / f1) + b * u128::from(denom / f2),
                    denom,
                }
            }
            _ => Self::Inf,
        }
    }
}

impl Sub for Duration {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Finite { num: a, denom: f1 }, Self::Finite { num: b, denom: f2 }) => {
                let denom = f1.lcm(&f2);
                Self::Finite {
                    num: a * u128::from(denom / f1) - b * u128::from(denom / f2),
                    denom,
                }
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

impl SubAssign for Duration {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}
