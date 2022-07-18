use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::ops::{BitAnd, BitOr, Not};

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct RelationTypeBit {
    inner: u8,
}

impl RelationTypeBit {
    pub fn new(r: u8) -> Self {
        Self { inner: r & 0b111 }
    }
}

impl Not for &RelationTypeBit {
    type Output = RelationTypeBit;

    fn not(self) -> Self::Output {
        (!self.inner & 0b111).into()
    }
}

impl Not for RelationTypeBit {
    type Output = RelationTypeBit;

    fn not(self) -> Self::Output {
        (&self).not()
    }
}

impl BitAnd for &RelationTypeBit {
    type Output = RelationTypeBit;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self.inner & rhs.inner).into()
    }
}

impl BitAnd for RelationTypeBit {
    type Output = RelationTypeBit;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self.inner & rhs.inner).into()
    }
}

impl BitOr for &RelationTypeBit {
    type Output = RelationTypeBit;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.inner | rhs.inner).into()
    }
}

impl BitOr for RelationTypeBit {
    type Output = RelationTypeBit;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.inner | rhs.inner).into()
    }
}

impl From<u8> for RelationTypeBit {
    fn from(u: u8) -> Self {
        Self { inner: u & 0b111 }
    }
}

impl From<&RelationType> for RelationTypeBit {
    fn from(r: &RelationType) -> Self {
        match r {
            RelationType::Eq => 0b001.into(),
            RelationType::GT => 0b100.into(),
            RelationType::LT => 0b010.into(),
            RelationType::GEq => 0b101.into(),
            RelationType::LEq => 0b011.into(),
            RelationType::Neq => 0b110.into(),
            RelationType::Tautology => 0b111.into(),
            RelationType::Contradiction => 0b000.into(),
        }
    }
}

impl From<RelationType> for RelationTypeBit {
    fn from(r: RelationType) -> Self {
        r.borrow().into()
    }
}

impl From<&RelationTypeBit> for RelationType {
    fn from(b: &RelationTypeBit) -> Self {
        match b.inner {
            0b000 => RelationType::Contradiction,
            0b001 => RelationType::Eq,
            0b010 => RelationType::LT,
            0b011 => RelationType::LEq,
            0b100 => RelationType::GT,
            0b101 => RelationType::GEq,
            0b110 => RelationType::Neq,
            0b111 => RelationType::Tautology,
            _ => unreachable!("RelationType bit is between 0b000 and 0b111"),
        }
    }
}

impl Display for RelationTypeBit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:03b}", self.inner)
    }
}

impl RelationTypeBit {
    /// Composition table: \
    /// ° | < | = | > \
    /// < | < | < | T \
    /// = | < | = | >
    /// > | T | > | >
    pub fn compose_simple_relation(x: &Self, y: &Self) -> Self {
        // diagonal of the table
        let x = x.inner;
        let y = y.inner;
        if x == 0b000 || y == 0b000 {
            Self { inner: 0b000 }
        } else if x == y {
            x.into()
        } else {
            match x | y {
                // = ° < => <
                0b011 => 0b010.into(),
                // = ° > => >
                0b101 => 0b100.into(),
                // < ° > => T
                0b110 => 0b111.into(),
                _ => unreachable!(),
            }
        }
    }

    pub fn vec_simple_relation(&self) -> Vec<RelationTypeBit> {
        let mut vec = vec![];
        for i in 0..3 {
            let r = self.inner & (0b001 << i);
            if r != 0 {
                vec.push(r.into())
            }
        }
        vec
    }

    //We suppose that composition is distributive
    pub fn compose(&self, other: &Self) -> Self {
        //println!("first: {:?}\nsecond: {:?}", self, other);
        let vec_1 = self.vec_simple_relation();
        let vec_2 = other.vec_simple_relation();
        //println!("vec_1: {:?}", vec_1);
        //println!("vec_2: {:?}", vec_2);
        let mut result = 0b000;
        for r1 in &vec_1 {
            for r2 in &vec_2 {
                result |= RelationTypeBit::compose_simple_relation(r1, r2).inner;
            }
        }
        result.into()
    }

    pub fn included_in(&self, other: &Self) -> bool {
        self | other == *other
    }

    pub fn intersect(&self, other: &Self) -> Self {
        self & other
    }

    pub fn converse(&self) -> Self {
        let r = self.inner;
        (((0b100 & r) >> 1) + ((0b010 & r) << 1) + (0b001 & r)).into()
    }
}

impl From<RelationTypeBit> for RelationType {
    fn from(rb: RelationTypeBit) -> Self {
        rb.borrow().into()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RelationType {
    Eq,
    GT,
    LT,
    GEq,
    LEq,
    Neq,
    Tautology,
    Contradiction,
}

impl Display for RelationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str: &str = match self {
            RelationType::Eq => "=",
            RelationType::GT => ">",
            RelationType::LT => "<",
            RelationType::GEq => ">=",
            RelationType::LEq => "<=",
            RelationType::Neq => "!=",
            RelationType::Tautology => "T",
            RelationType::Contradiction => "{}",
        };

        write!(f, "{}", str)
    }
}

impl BitAnd for &RelationType {
    type Output = RelationType;

    fn bitand(self, rhs: Self) -> Self::Output {
        (RelationTypeBit::from(self) & RelationTypeBit::from(rhs)).into()
    }
}

impl BitOr for &RelationType {
    type Output = RelationType;

    fn bitor(self, rhs: Self) -> Self::Output {
        (RelationTypeBit::from(self) | RelationTypeBit::from(rhs)).into()
    }
}

impl BitAnd for RelationType {
    type Output = RelationType;

    fn bitand(self, rhs: Self) -> Self::Output {
        (RelationTypeBit::from(self) & RelationTypeBit::from(rhs)).into()
    }
}

impl BitOr for RelationType {
    type Output = RelationType;

    fn bitor(self, rhs: Self) -> Self::Output {
        (RelationTypeBit::from(self) | RelationTypeBit::from(rhs)).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_relation_bit_and() {
        let _contradiction: RelationTypeBit = 0b000.into();
        let eq: RelationTypeBit = 0b001.into();
        let _lt: RelationTypeBit = 0b010.into();
        let leq: RelationTypeBit = 0b011.into();
        let _gt: RelationTypeBit = 0b100.into();
        let geq: RelationTypeBit = 0b101.into();
        let _neq: RelationTypeBit = 0b110.into();
        let _tautology: RelationTypeBit = 0b111.into();
        assert_eq!(eq, &eq & &leq);
        assert_eq!(eq, &eq & &geq);
    }

    #[test]
    pub fn test_relation_bit_or() {
        let _contradiction: RelationTypeBit = 0b000.into();
        let eq: RelationTypeBit = 0b001.into();
        let lt: RelationTypeBit = 0b010.into();
        let leq: RelationTypeBit = 0b011.into();
        let gt: RelationTypeBit = 0b100.into();
        let geq: RelationTypeBit = 0b101.into();
        let _neq: RelationTypeBit = 0b110.into();
        let _tautology: RelationTypeBit = 0b111.into();
        assert_eq!(leq, &eq | &lt);
        assert_eq!(geq, &eq | &gt);
    }

    #[test]
    pub fn test_relation_and() {
        assert_eq!(RelationType::Eq, &RelationType::Eq & &RelationType::LEq);
        assert_eq!(RelationType::Eq, &RelationType::Eq & &RelationType::GEq);
    }

    #[test]
    pub fn test_relation_or() {
        assert_eq!(RelationType::LEq, &RelationType::Eq | &RelationType::LT);
        assert_eq!(RelationType::GEq, &RelationType::Eq | &RelationType::GT);
    }

    #[test]
    pub fn test_compose_unit() {
        //= ° = -> =
        assert_eq!(
            RelationTypeBit::from(0b001),
            RelationTypeBit::compose_simple_relation(&0b001.into(), &0b001.into())
        );
        // < ° < -> <
        assert_eq!(
            RelationTypeBit::from(0b010),
            RelationTypeBit::compose_simple_relation(&0b010.into(), &0b010.into())
        );
        // > ° > -> >
        assert_eq!(
            RelationTypeBit::from(0b100),
            RelationTypeBit::compose_simple_relation(&0b100.into(), &0b100.into())
        );
        // < ° > -> T
        assert_eq!(
            RelationTypeBit::from(0b111),
            RelationTypeBit::compose_simple_relation(&0b010.into(), &0b100.into())
        );
        // < ° = -> <
        assert_eq!(
            RelationTypeBit::from(0b010),
            RelationTypeBit::compose_simple_relation(&0b001.into(), &0b010.into())
        );
        // > ° = -> >
        assert_eq!(
            RelationTypeBit::from(0b100),
            RelationTypeBit::compose_simple_relation(&0b001.into(), &0b100.into())
        );
    }

    #[test]
    pub fn test_compose() {
        // >= ° > -> >
        assert_eq!(
            RelationTypeBit::from(0b010),
            RelationTypeBit::from(0b011).compose(&RelationTypeBit::from(0b010))
        );
        // >= ° < -> T
        assert_eq!(
            RelationTypeBit::from(0b111),
            RelationTypeBit::from(0b101).compose(&RelationTypeBit::from(0b010))
        );
    }

    #[test]
    pub fn test_not() {
        // !(=) -> !=        assert_eq!(RelationTypeBit::from(0b001), RelationType::GT.compose())

        assert_eq!(RelationTypeBit::from(0b110), !RelationTypeBit::from(0b001));
        // !(<) -> >=
        assert_eq!(RelationTypeBit::from(0b101), !RelationTypeBit::from(0b010));
    }

    #[test]
    pub fn test_converse() {
        //converse(contradiction) => contradiction
        assert_eq!(
            RelationTypeBit::from(0b000),
            RelationTypeBit::from(0b000).converse()
        );
        //converse(=) => =
        assert_eq!(
            RelationTypeBit::from(0b001),
            RelationTypeBit::from(0b001).converse()
        );
        //converse(<) => >
        assert_eq!(
            RelationTypeBit::from(0b010),
            RelationTypeBit::from(0b100).converse()
        );
        //converse(>) => <
        assert_eq!(
            RelationTypeBit::from(0b100),
            RelationTypeBit::from(0b010).converse()
        );
        //converse(<=) => >=
        assert_eq!(
            RelationTypeBit::from(0b101),
            RelationTypeBit::from(0b011).converse()
        );
        //converse(>=) => <=
        assert_eq!(
            RelationTypeBit::from(0b011),
            RelationTypeBit::from(0b101).converse()
        );
        //converse(!=) => !=
        assert_eq!(
            RelationTypeBit::from(0b110),
            RelationTypeBit::from(0b110).converse()
        );
        //converse(tautology) => tautology
        assert_eq!(
            RelationTypeBit::from(0b111),
            RelationTypeBit::from(0b111).converse()
        );
    }
}
