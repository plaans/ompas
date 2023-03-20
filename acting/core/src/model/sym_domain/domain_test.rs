use crate::model::sym_domain::basic_type::BasicType;
use crate::model::sym_domain::domain_test::DomainTest::*;
use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_domain::{cst, Domain};
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
    False,
    Nil,
    Number,
    Int,
    Float,
    New(String),
    Substract(Box<DomainTest>, Box<DomainTest>),
    Cst(Box<DomainTest>, cst::Cst),
    Proc,
    Primitive,
    Fn,
    Lambda,
    Application(Box<DomainTest>, Vec<DomainTest>, Box<DomainTest>),
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
            False => write!(f, "false"),
            Proc => write!(f, "proc"),
            Primitive => write!(f, "primitive"),
            Fn => write!(f, "fn"),
            Lambda => write!(f, "lambda"),
            Application(t, params, r) => {
                let mut str = "(".to_string();
                for (i, d) in params.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{d}")?;
                }
                str.push(')');
                write!(f, "{t}:{str} -> {r}")
            }
        }
    }
}

impl DomainTest {
    pub fn into_domain(&self, dc: &TypeLattice) -> Domain {
        match self {
            Any => BasicType::Any.into(),
            Empty => BasicType::Empty.into(),
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
            Map => BasicType::Map.into(),
            List(t) => match t {
                None => BasicType::List.into(),
                Some(types) => Domain::Composed(
                    BasicType::List as usize,
                    types.iter().map(|t| t.into_domain(dc)).collect(),
                ),
            },
            //Type::Vector(_) => {}
            //Type::Tuple(_) => {}
            Err(t) => match t {
                None => BasicType::Err.into(),
                Some(t) => Domain::Composed(BasicType::Err as usize, vec![t.into_domain(dc)]),
            },
            Handle(t) => match t {
                None => BasicType::Handle.into(),
                Some(t) => Domain::Composed(BasicType::Handle as usize, vec![t.into_domain(dc)]),
            },
            //Type::Alias(_, _) => {}
            Symbol => BasicType::Symbol.into(),
            Boolean => BasicType::Boolean.into(),
            True => BasicType::True.into(),
            Nil => Domain::nil(),
            Number => BasicType::Number.into(),
            Int => BasicType::Int.into(),
            Float => BasicType::Float.into(),
            Substract(s, t) => {
                let s = s.into_domain(dc);
                let t = t.into_domain(dc);
                dc.__substract(&s, &t)
            }
            Cst(d1, c1) => Domain::Cst(Box::new(d1.into_domain(dc)), c1.clone()),
            New(s) => Domain::Simple(*dc.get_type_id(s.as_str()).unwrap()),
            _ => todo!(),
        }
    }

    pub fn from_domain(_dc: &TypeLattice, t: &Domain) -> Self {
        match t {
            Domain::Simple(t) => match BasicType::try_from(*t) {
                Ok(t) => match t {
                    BasicType::Empty => Empty,
                    BasicType::Any => Any,
                    BasicType::Boolean => Boolean,
                    BasicType::List => List(None),
                    BasicType::Map => Map,
                    BasicType::Err => Err(None),
                    BasicType::Handle => Handle(None),
                    BasicType::Number => Number,
                    BasicType::Int => Int,
                    BasicType::Float => Float,
                    BasicType::Symbol => Symbol,
                    BasicType::EmptyList => Nil,
                    BasicType::True => True,
                    BasicType::False => False,
                    BasicType::Nil => Nil,
                    BasicType::Proc => Proc,
                    BasicType::Primitive => Primitive,
                    BasicType::Fn => Fn,
                    BasicType::Lambda => Lambda,
                    BasicType::Vector => Vector(None),
                    BasicType::Tuple => Tuple(None),
                },
                Result::Err(_) => New(_dc.format_type(t)),
            },

            Domain::Composed(t, sub) => {
                let t = BasicType::try_from(*t).unwrap();
                match t {
                    BasicType::Err => {
                        assert_eq!(sub.len(), 1);
                        Err(Some(Box::new(DomainTest::from_domain(_dc, &sub[0]))))
                    }
                    BasicType::Handle => {
                        assert_eq!(sub.len(), 1);
                        Handle(Some(Box::new(DomainTest::from_domain(_dc, &sub[0]))))
                    }
                    BasicType::List => List(Some(
                        sub.iter()
                            .map(|t| DomainTest::from_domain(_dc, t))
                            .collect(),
                    )),
                    _ => panic!(),
                }
            }
            Domain::Union(t) => Union(t.iter().map(|t| DomainTest::from_domain(_dc, t)).collect()),
            Domain::Substract(t1, t2) => Substract(
                Box::new(DomainTest::from_domain(_dc, t1)),
                Box::new(DomainTest::from_domain(_dc, t2)),
            ),
            Domain::Cst(t, c) => Cst(Box::new(DomainTest::from_domain(_dc, t)), c.clone()),
            Domain::Application(t, params, r) => Application(
                Box::new(DomainTest::from_domain(_dc, t)),
                params
                    .iter()
                    .map(|t| DomainTest::from_domain(_dc, t))
                    .collect(),
                Box::new(DomainTest::from_domain(_dc, r)),
            ),
        }
    }

    pub fn meet(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(tl, &tl.__meet(&ta, &tb))
    }

    pub fn union(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(tl, &tl.__union(&ta, &tb))
    }

    pub fn substract(tl: &TypeLattice, ta: &DomainTest, tb: &DomainTest) -> DomainTest {
        let ta = ta.into_domain(tl);
        let tb = tb.into_domain(tl);

        DomainTest::from_domain(tl, &tl.__substract(&ta, &tb))
    }
}
