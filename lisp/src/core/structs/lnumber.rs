use serde::*;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Sub};

/// Representation of numbers il LValue:
/// - Int(i64)
/// - Float(f64)
/// - Usize(usize)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LNumber {
    Int(i64),
    Float(f64),
    Usize(usize),
}

impl LNumber {
    pub fn is_real(&self) -> bool {
        true
    }
    pub fn is_natural(&self) -> bool {
        match self {
            LNumber::Usize(_) => true,
            LNumber::Int(i) => i.is_positive(),
            LNumber::Float(f) => (f.floor() - *f).abs() < f64::EPSILON,
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            LNumber::Int(_) | LNumber::Usize(_) => true,
            LNumber::Float(f) => f.is_sign_positive(),
        }
    }
}

impl Display for LNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LNumber::Int(i) => write!(f, "{}", i),
            LNumber::Float(fl) => write!(f, "{}", fl),
            LNumber::Usize(u) => write!(f, "{}", u),
        }
    }
}

impl From<&LNumber> for String {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => i.to_string(),
            LNumber::Float(f) => f.to_string(),
            LNumber::Usize(u) => u.to_string(),
        }
    }
}

impl From<LNumber> for String {
    fn from(n: LNumber) -> Self {
        (&n).to_string()
    }
}

impl PartialEq for LNumber {
    fn eq(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 == n2
    }
}

impl From<&LNumber> for usize {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i as usize,
            LNumber::Float(f) => *f as usize,
            LNumber::Usize(u) => *u,
        }
    }
}

impl From<&LNumber> for f64 {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i as f64,
            LNumber::Float(f) => *f,
            LNumber::Usize(u) => *u as f64,
        }
    }
}

impl From<&LNumber> for i64 {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i,
            LNumber::Float(f) => *f as i64,
            LNumber::Usize(u) => *u as i64,
        }
    }
}

impl From<i64> for LNumber {
    fn from(i: i64) -> Self {
        LNumber::Int(i)
    }
}

impl From<i32> for LNumber {
    fn from(i: i32) -> Self {
        LNumber::Int(i as i64)
    }
}

impl From<f64> for LNumber {
    fn from(f: f64) -> Self {
        LNumber::Float(f)
    }
}

impl From<f32> for LNumber {
    fn from(f: f32) -> Self {
        LNumber::Float(f as f64)
    }
}

impl From<usize> for LNumber {
    fn from(u: usize) -> Self {
        LNumber::Usize(u)
    }
}

impl Hash for LNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LNumber::Int(i) => i.hash(state),
            LNumber::Float(f) => f.to_string().hash(state),
            LNumber::Usize(u) => u.hash(state),
        }
    }
}

impl PartialOrd for LNumber {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 < n2
    }

    fn le(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 <= n2
    }

    fn gt(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 > n2
    }

    fn ge(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 >= n2
    }
}

impl Add for &LNumber {
    type Output = LNumber;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 + *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 + *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 + *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 + *i2 as f64),
            (_, _) => unimplemented!(),
        }
    }
}

impl Sub for &LNumber {
    type Output = LNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 - *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 - *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 - *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 - *i2 as f64),
            (_, _) => unimplemented!(),
        }
    }
}

impl Div for &LNumber {
    type Output = LNumber;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 / *i2),
            (n1, LNumber::Float(f2)) => LNumber::Float(f64::from(n1) / *f2),
            (LNumber::Float(f1), n2) => LNumber::Float(*f1 / f64::from(n2)),
            (n1, LNumber::Int(i2)) => LNumber::Int(i64::from(n1) / *i2),
            (LNumber::Int(i1), n2) => LNumber::Int(*i1 / i64::from(n2)),
            (n1, n2) => panic!("attempted rare case of division with {:?} and {:?}", n1, n2),
        }
    }
}

impl Mul for &LNumber {
    type Output = LNumber;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 * *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 * *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 * *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 * *i2 as f64),
            (_, _) => unimplemented!(),
        }
    }
}

impl Add for LNumber {
    type Output = LNumber;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}
impl Sub for LNumber {
    type Output = LNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}
impl Mul for LNumber {
    type Output = LNumber;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Div for LNumber {
    type Output = LNumber;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

impl Eq for LNumber {}

#[cfg(test)]
mod test {
    use super::*;

    fn test_add() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert_eq!(LNumber::Int(8), &i1 + &i2);
        assert_eq!(LNumber::Float(8.0), &i1 + &f2);
        assert_eq!(LNumber::Float(8.0), &f1 + &f2);
    }

    fn test_sub() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert_eq!(LNumber::Int(-2), &i1 - &i2);
        assert_eq!(LNumber::Float(-2.0), &i1 - &f2);
        assert_eq!(LNumber::Float(-2.0), &f1 - &f2);
    }

    fn test_mul() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert_eq!(LNumber::Int(15), &i1 * &i2);
        assert_eq!(LNumber::Float(15.0), &i1 * &f2);
        assert_eq!(LNumber::Float(15.0), &f1 * &f2);
    }

    fn test_div() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert_eq!(LNumber::Int(0), &i1 / &i2);
        assert_eq!(LNumber::Float(0.6), &i1 / &f2);
        assert_eq!(LNumber::Float(0.6), &f1 / &f2);
    }

    #[test]
    fn test_math() {
        test_add();
        test_sub();
        test_div();
        test_mul();
    }

    fn test_gt() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert!(!(&i1 > &i2));
        assert!(&i2 > &i1);
        assert!(!(&i2 > &i2));
        assert!(!(&f1 > &f2));
        assert!(&f2 > &f1);
        assert!(!(&f2 > &f2));
        assert!(&i2 > &f1);
    }

    fn test_lt() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert!(&i1 < &i2);
        assert!(!(&i2 < &i1));
        assert!(!(&i2 < &i2));
        assert!(&f1 < &f2);
        assert!(!(&f2 < &f1));
        assert!(!(&f2 < &f2));
        assert!(&i1 < &f2);
    }

    fn test_ge() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert!(!(&i1 >= &i2));
        assert!(&i2 >= &i1);
        assert!(&i2 >= &i2);
        assert!(!(&f1 >= &f2));
        assert!(&f2 >= &f1);
        assert!(&f2 >= &f2);
        assert!(&i2 >= &f2);
    }

    fn test_le() {
        let i1: LNumber = 3.into();
        let i2: LNumber = 5.into();
        let f1: LNumber = 3.0.into();
        let f2: LNumber = 5.0.into();
        assert!(&i1 <= &i2);
        assert!(!(&i2 <= &i1));
        assert!(&i2 <= &i2);
        assert!(&f1 <= &f2);
        assert!(!(&f2 <= &f1));
        assert!(&f2 <= &f2);
        assert!(&i2 <= &f2);
    }

    #[test]
    fn test_ord() {
        test_gt();
        test_ge();
        test_lt();
        test_le();
    }
}
