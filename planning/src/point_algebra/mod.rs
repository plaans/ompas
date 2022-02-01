use crate::point_algebra::RelationType::*;
use im::HashMap;
use std::collections::HashSet;
use std::ops::{BitAnd, BitOr};

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct RelationTypeBit {
    inner: u8,
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
        Self { inner: u }
    }
}

impl From<&RelationType> for RelationTypeBit {
    fn from(r: &RelationType) -> Self {
        match r {
            Eq => 0b001.into(),
            GT => 0b100.into(),
            LT => 0b010.into(),
            GEq => 0b101.into(),
            LEq => 0b011.into(),
            Neq => 0b110.into(),
            Tautology => 0b111.into(),
            Contradiction => 0b000.into(),
        }
    }
}

impl From<RelationType> for RelationTypeBit {
    fn from(r: RelationType) -> Self {
        (&r).into()
    }
}

impl From<&RelationTypeBit> for RelationType {
    fn from(b: &RelationTypeBit) -> Self {
        match b.inner {
            0b000 => Contradiction,
            0b001 => Eq,
            0b010 => LT,
            0b011 => LEq,
            0b100 => GT,
            0b101 => GEq,
            0b110 => Neq,
            0b111 => Tautology,
            _ => unreachable!("RelationType bit is between 0b000 and 0b111"),
        }
    }
}

impl From<RelationTypeBit> for RelationType {
    fn from(rb: RelationTypeBit) -> Self {
        (&rb).into()
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

pub struct Relation<T> {
    i: T,
    j: T,
    RelationType: RelationType,
}

pub struct Problem<T> {
    inner: Vec<Relation<T>>,
}

impl<T: PartialEq> From<Problem<T>> for PAP<T> {
    fn from(problem: Problem<T>) -> Self {
        todo!()
    }
}

pub struct PAP<T> {
    hashmap: HashMap<usize, T>,
    graph: Graph,
}

pub struct Graph {
    inner: Vec<Vec<RelationTypeBit>>,
}

#[cfg(test)]
mod tests {
    use crate::point_algebra::{RelationType, RelationTypeBit};

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
}
