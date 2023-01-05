use crate::structs::domain::domain_test::DomainTest::*;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::type_lattice::TypeLattice;
use crate::structs::domain::{cst, Domain};
use std::collections::HashSet;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum DomainTest {
    Any,
    Empty,
    Union(Vec<DomainTest>),
    Map,
    List(Option<Vec<DomainTest>>),
    Vector(Option<Box<DomainTest>>),
    Tuple(Option<Vec<DomainTest>>),
    Handle(Option<Box<DomainTest>>),
    Err(Option<Box<DomainTest>>),
    Alias(Box<DomainTest>, Box<DomainTest>),
    //Literal,
    Symbol,
    Boolean,
    True,
    Nil,
    Number,
    Int,
    Float,
    New(String),
    Substract(Box<DomainTest>, Box<DomainTest>),
    Cst(Box<DomainTest>, cst::Cst),
}

impl From<i64> for DomainTest {
    fn from(i: i64) -> Self {
        Self::Cst(Box::new(Int), cst::Cst::Int(i))
    }
}

impl From<f64> for DomainTest {
    fn from(f: f64) -> Self {
        Self::Cst(Box::new(Float), cst::Cst::Float(f))
    }
}

impl From<String> for DomainTest {
    fn from(s: String) -> Self {
        Self::Cst(Box::new(Symbol), cst::Cst::Symbol(s))
    }
}

impl From<bool> for DomainTest {
    fn from(b: bool) -> Self {
        match b {
            true => True,
            false => Nil,
        }
    }
}

impl Display for DomainTest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "Any"),
            Boolean => write!(f, "Boolean"),
            Number => write!(f, "Number"),
            Int => write!(f, "Int"),
            Float => write!(f, "Float"),
            List(t) => match t {
                None => write!(f, "List"),
                Some(types) => {
                    let mut str = "(".to_string();
                    for (i, t) in types.iter().enumerate() {
                        if i != 0 {
                            str.push(',');
                        }
                        write!(str, "{t}").unwrap();
                    }
                    write!(f, "{str})")
                }
            },
            Map => write!(f, "Map"),
            Symbol => write!(f, "Symbol"),
            New(s) => write!(f, "{s}"),
            Empty => write!(f, "Empty"),
            Handle(h) => match h {
                None => write!(f, "Handle"),
                Some(t) => write!(f, "Handle<{t}>"),
            },
            Err(e) => match e {
                None => write!(f, "Err"),
                Some(e) => write!(f, "Err<{e}>"),
            },
            Union(types) => match types.is_empty() {
                true => {
                    write!(f, "{}", Empty)
                }
                false => {
                    write!(f, "{{")?;
                    for (i, t) in types.iter().enumerate() {
                        if i != 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, "}}")
                }
            },
            Vector(t) => match t {
                None => write!(f, "Vector"),
                Some(t) => write!(f, "Vector<{t}>"),
            },
            Tuple(types) => match types {
                None => write!(f, "Tuple"),
                Some(types) => {
                    write!(f, "Tuple<")?;
                    for (i, t) in types.iter().enumerate() {
                        if i != 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, ">")
                }
            },
            /*Cst(cst) => {
                write!(f, "{}[{}]", cst.r#type, cst.value)
            }*/
            Alias(n, _) => write!(f, "{n}"),
            Substract(t1, t2) => write!(f, "({t1} / {t2})"),
            Cst(d1, c) => write!(f, "{d1}[{c}]"),
            True => write!(f, "true"),
            Nil => write!(f, "nil"),
        }
    }
}

impl DomainTest {
    pub fn into_domain(&self, dc: &TypeLattice) -> Domain {
        match self {
            Any => RootType::Any.into(),
            Empty => RootType::Empty.into(),
            Union(vec) => {
                let mut types: HashSet<Domain> = Default::default();
                for t in vec {
                    match t.into_domain(dc) {
                        Domain::Union(vec) => {
                            for t in vec {
                                types.insert(t);
                            }
                        }
                        t => {
                            types.insert(t);
                        }
                    };
                }
                dc.simplify_union(types)
            }
            Map => RootType::Map.into(),
            List(t) => match t {
                None => RootType::List.into(),
                Some(types) => Domain::Composed(
                    RootType::List as usize,
                    types.iter().map(|t| t.into_domain(dc)).collect(),
                ),
            },
            //Type::Vector(_) => {}
            //Type::Tuple(_) => {}
            Err(t) => match t {
                None => RootType::Err.into(),
                Some(t) => Domain::Composed(RootType::Err as usize, vec![t.into_domain(dc)]),
            },
            Handle(t) => match t {
                None => RootType::Handle.into(),
                Some(t) => Domain::Composed(RootType::Handle as usize, vec![t.into_domain(dc)]),
            },
            //Type::Alias(_, _) => {}
            Symbol => RootType::Symbol.into(),
            Boolean => RootType::Boolean.into(),
            True => RootType::True.into(),
            Nil => Domain::nil(),
            Number => RootType::Number.into(),
            Int => RootType::Int.into(),
            Float => RootType::Float.into(),
            Substract(s, t) => {
                let s = s.into_domain(dc);
                let t = t.into_domain(dc);
                dc.__substract(&s, &t)
            }
            Cst(d1, c1) => Domain::Cst(Box::new(d1.into_domain(dc)), c1.clone()),
            _ => unreachable!(),
        }
    }

    pub fn from_domain(dc: &TypeLattice, t: &Domain) -> Self {
        match t {
            Domain::Simple(t) => match RootType::try_from(*t) {
                Ok(t) => match t {
                    RootType::Empty => Empty,
                    RootType::Any => Any,
                    RootType::Boolean => Boolean,
                    RootType::List => List(None),
                    RootType::Map => Map,
                    RootType::Err => Err(None),
                    RootType::Handle => Handle(None),
                    RootType::Number => Number,
                    RootType::Int => Int,
                    RootType::Float => Float,
                    RootType::Symbol => Symbol,
                    RootType::EmptyList => Nil,
                    RootType::True => True,
                    RootType::False => Nil,
                },
                Result::Err(_) => Empty,
            },

            Domain::Composed(t, sub) => {
                let t = RootType::try_from(*t).unwrap();
                match t {
                    RootType::Err => {
                        assert_eq!(sub.len(), 1);
                        Err(Some(Box::new(DomainTest::from_domain(&dc, &sub[0]))))
                    }
                    RootType::Handle => {
                        assert_eq!(sub.len(), 1);
                        Handle(Some(Box::new(DomainTest::from_domain(&dc, &sub[0]))))
                    }
                    RootType::List => List(Some(
                        sub.iter()
                            .map(|t| DomainTest::from_domain(&dc, t))
                            .collect(),
                    )),
                    _ => panic!(),
                }
            }
            Domain::Union(t) => Union(t.iter().map(|t| DomainTest::from_domain(dc, t)).collect()),
            Domain::Substract(t1, t2) => Substract(
                Box::new(DomainTest::from_domain(dc, t1)),
                Box::new(DomainTest::from_domain(dc, t2)),
            ),
            Domain::Cst(t, c) => Cst(Box::new(DomainTest::from_domain(dc, t)), c.clone()),
        }
    }

    pub fn meet(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(&tl, &tl.__meet(&ta, &tb))
    }

    pub fn union(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(&tl, &tl.__union(&ta, &tb))
    }

    pub fn substract(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(&tl, &tl.__substract(&ta, &tb))
    }
}
