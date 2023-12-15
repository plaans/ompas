use std::ops::{Add, Deref, Div, Mul, Sub};

pub mod config;
pub mod formatter;
pub mod instance;
mod plot;
pub mod problem;
pub mod system;

#[derive(Default, Copy, Clone)]
pub struct Stat {
    mean: f64,
    sd: f64,
    se: f64,
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
            mean: t.into(),
            sd: 0.0,
            se: 0.0,
        }
    }

    pub fn abs(&self) -> Self {
        Self {
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
            mean: self.mean - rhs.mean,
            sd: (self.sd.powi(2) + rhs.sd.powi(2)).sqrt(),
            se: (self.se.powi(2) + rhs.se.powi(2)).sqrt(),
        }
    }
}

impl<T: Into<f64>> From<&[T]> for Stat {
    fn from(value: &[T]) -> Self {
        let n = value.len();
        let values: Vec<f64> = value.iter().map(|t| t.into()).collect();
        let mean = values.iter().sum::<f64>() / n as f64;
        let standard_deviation =
            (values.iter().map(|f| (f - mean).powi(2)).sum::<f64>() / (n as f64 - 1.0)).sqrt();
        Self {
            mean,
            sd: standard_deviation,
            se: standard_deviation / (n as f64).sqrt(),
        }
    }
}
