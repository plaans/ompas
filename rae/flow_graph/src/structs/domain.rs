use crate::structs::domain::SimpleType::*;
use crate::structs::domain::TypeR::*;
use crate::structs::r#type::Type;
use sompas_core::modules::error::check;
use std::collections::{HashMap, VecDeque};
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use std::ops::Deref;

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq)]
enum SimpleType {
    Empty = 0,
    Any = 1,
    Boolean = 2,
    List = 3,
    True = 4,
    Nil = 5,
    Map = 6,
    Err = 7,
    Handle = 8,
    Number = 9,
    Int = 10,
    Float = 11,
    Symbol = 12,
}

impl TryFrom<DomainId> for SimpleType {
    type Error = ();

    fn try_from(value: DomainId) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Empty,
            1 => Any,
            2 => Boolean,
            3 => List,
            4 => True,
            5 => Nil,
            6 => Map,
            7 => Err,
            8 => Handle,
            9 => Number,
            10 => Int,
            11 => Float,
            12 => Symbol,
            _ => return Result::Err(()),
        })
    }
}

impl Display for SimpleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "Any"),
            Boolean => write!(f, "Boolean"),
            True => write!(f, "True"),
            Nil => write!(f, "Nil"),
            List => write!(f, "List"),
            Empty => write!(f, "Empty"),
            Map => write!(f, "Map"),
            Err => write!(f, "Err"),
            Handle => write!(f, "Handle"),
            Number => write!(f, "Number"),
            Int => write!(f, "Int"),
            Float => write!(f, "Float"),
            Symbol => write!(f, "Symbol"),
        }
    }
}

enum Domain {
    Type(SimpleType),
}

impl Display for Domain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Domain::Type(t) => write!(f, "{}", t),
        }
    }
}

pub struct DomainCollection {
    domains: Vec<Domain>,
    childs: Vec<Vec<DomainId>>,
    parents: Vec<Vec<DomainId>>,
    decomposition: Vec<Vec<DomainId>>,
    ids: HashMap<Domain, DomainId>,
}

pub type DomainId = usize;

#[derive(Clone, PartialEq, Eq)]
pub enum TypeR {
    Simple(DomainId),
    Composed(DomainId, Vec<TypeR>),
    Union(Vec<TypeR>),
}

impl TypeR {
    pub fn from_type(t: &Type) -> Self {
        match t {
            Type::Any => Simple(Any as usize),
            Type::Empty => Simple(Empty as usize),
            Type::Union(vec) => Union(vec.iter().map(|t| TypeR::from_type(t)).collect()),
            Type::Map => Simple(Map as usize),
            Type::List => Simple(List as usize),
            //Type::Vector(_) => {}
            //Type::Tuple(_) => {}
            Type::Err(t) => match t.deref() {
                Type::Any => Simple(Err as usize),
                t => Composed(Err as usize, vec![TypeR::from_type(t)]),
            },
            Type::Handle(t) => match t.deref() {
                Type::Any => Simple(Handle as usize),
                t => Composed(Handle as usize, vec![TypeR::from_type(t)]),
            },
            //Type::Alias(_, _) => {}
            Type::Symbol => Simple(Symbol as usize),
            Type::Boolean => Simple(Boolean as usize),
            Type::True => Simple(True as usize),
            Type::Nil => Simple(Nil as usize),
            Type::Number => Simple(Number as usize),
            Type::Int => Simple(Int as usize),
            Type::Float => Simple(Float as usize),
            //Type::New(_) => {}
            _ => Simple(Empty as usize),
        }
    }

    pub fn into_type(t: &Self) -> Type {
        match t {
            Simple(t) => match SimpleType::try_from(*t) {
                Ok(t) => match t {
                    Empty => Type::Empty,
                    Any => Type::Any,
                    Boolean => Type::Boolean,
                    List => Type::List,
                    True => Type::True,
                    Nil => Type::Nil,
                    Map => Type::Map,
                    Err => Type::Err(Box::new(Type::Any)),
                    Handle => Type::Handle(Box::new(Type::Any)),
                    Number => Type::Number,
                    Int => Type::Int,
                    Float => Type::Float,
                    Symbol => Type::Symbol,
                    _ => Type::Empty,
                },
                Result::Err(_) => Type::Empty,
            },

            Composed(t, sub) => {
                let t = SimpleType::try_from(*t).unwrap();
                match t {
                    Err => {
                        assert_eq!(sub.len(), 1);
                        Type::Err(Box::new(TypeR::into_type(&sub[0])))
                    }
                    Handle => {
                        assert_eq!(sub.len(), 1);
                        Type::Err(Box::new(TypeR::into_type(&sub[0])))
                    }
                    _ => panic!(),
                }
            }
            Union(t) => Type::Union(t.iter().map(|t| TypeR::into_type(t)).collect()),
        }
    }
}

impl From<SimpleType> for TypeR {
    fn from(t: SimpleType) -> Self {
        Simple(t as usize)
    }
}

impl From<DomainId> for TypeR {
    fn from(id: DomainId) -> Self {
        Simple(id)
    }
}

impl TypeR {
    pub fn format(&self, dc: &DomainCollection) -> String {
        match self {
            Simple(id) => dc.domains[*id].to_string(),
            Composed(id, vec) => {
                format!("{}<{}>", dc.domains[*id].to_string(), {
                    let mut str = "".to_string();
                    for (i, d) in vec.iter().enumerate() {
                        if i != 0 {
                            str.push(',');
                        }
                        write!(str, "{}", d.format(dc));
                    }
                    str
                })
            }
            Union(u) => {
                let mut str = "{".to_string();
                for (i, d) in u.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{}", d.format(dc));
                }
                str.push('}');
                str
            }
        }
    }
}

impl Default for DomainCollection {
    fn default() -> Self {
        let mut dc = Self {
            domains: vec![Domain::Type(Empty), Domain::Type(Any)],
            childs: vec![vec![], vec![]],
            parents: vec![vec![], vec![]],
            decomposition: vec![vec![], vec![]],
            ids: Default::default(),
        };

        dc.add_type(Boolean, vec![]);
        dc.add_type(List, vec![]);
        dc.add_type(True, vec![Boolean as usize]);
        dc.add_type(Nil, vec![List as usize, Boolean as usize]);
        dc.add_type(Map, vec![]);
        dc.add_type(Err, vec![]);
        dc.add_type(Handle, vec![]);
        dc.add_type(Number, vec![]);
        dc.add_type(Int, vec![Number as usize]);
        dc.add_type(Float, vec![Number as usize]);
        dc.add_type(Symbol, vec![]);
        dc
    }
}

const VERTICE_PREFIX: &str = "D";

impl DomainCollection {
    fn add_type(&mut self, r#type: impl Into<SimpleType>, parents: Vec<DomainId>) -> DomainId {
        let r#type = r#type.into();
        let id = self.domains.len();
        self.domains.push(Domain::Type(r#type));
        self.childs.push(vec![]);
        self.parents.push(vec![]);
        self.decomposition.push(vec![]);
        if parents.is_empty() {
            self.add_parent(id, Any as usize)
        } else {
            for parent in parents {
                self.add_parent(id, parent)
            }
        }
        id
    }

    fn add_parent(&mut self, id_type: DomainId, id_parent: DomainId) {
        let parents = &mut self.parents[id_type];
        if !parents.is_empty() {
            if parents[0] == Any as usize {
                parents.remove(0);
            }
        }
        parents.push(id_parent);
        let childs = &mut self.childs[id_parent];
        childs.push(id_type);
    }

    pub fn get_all_childs(&self, id: &DomainId) -> Vec<DomainId> {
        let mut queue = VecDeque::default();
        queue.push_front(*id);
        let mut childs = vec![];

        while let Some(id) = queue.pop_front() {
            if !childs.contains(&id) {
                childs.push(id);
                for id in &self.childs[id] {
                    queue.push_back(*id);
                }
            }
        }

        childs
    }

    pub fn meet(&self, ta: &Type, tb: &Type) -> Type {
        let ta = TypeR::from_type(&ta);
        let tb = TypeR::from_type(&tb);

        TypeR::into_type(&self.__meet(&ta, &tb))
    }

    fn __meet(&self, t1: &TypeR, t2: &TypeR) -> TypeR {
        match (t1, t2) {
            (Simple(t1), Simple(t2)) => {
                if t1 == t2 {
                    Simple(*t1)
                } else {
                    let childs_t1 = self.get_all_childs(t1);
                    //println!("childs of {t1}: {:?}", childs_t1);
                    let childs_t2 = self.get_all_childs(t2);
                    //println!("childs of {t2}: {:?}", childs_t2);

                    for child_1 in &childs_t1 {
                        for child_2 in &childs_t2 {
                            if child_1 == child_2 {
                                return Simple(*child_1);
                            }
                        }
                    }
                    return Simple(Empty as usize);
                }
            }
            (Simple(_), Composed(top2, _)) => {
                let nt2 = Simple(*top2);
                let meet = self.__meet(t1, &nt2);
                if nt2 == meet {
                    t2.clone()
                } else {
                    meet
                }
            }
            (Composed(top1, _), Simple(_)) => {
                let nt1 = Simple(*top1);
                let meet = self.__meet(&nt1, t2);
                if nt1 == meet {
                    t2.clone()
                } else {
                    meet
                }
            }
            (Composed(top1, comp1), Composed(top2, comp2)) => {
                if top1 == top2 && comp1.len() == comp2.len() {
                    let mut comp = vec![];
                    for (t1, t2) in comp1.iter().zip(comp2) {
                        let meet = self.__meet(t1, t2);
                        if meet == Simple(Empty as usize) {
                            return Simple(Empty as usize);
                        }
                        comp.push(meet)
                    }
                    Composed(top1.clone(), comp)
                } else {
                    self.__meet(&Simple(*top1), &Simple(*top2))
                }
            }
            (Union(ua), Union(ub)) => {
                //println!("Meet {ta} and {tb}.");
                let mut union = vec![];
                for ta in ua {
                    //println!("Meet {ta} and {tb}.");
                    let meet = self.__meet(ta, t2);
                    if meet != Simple(Empty as usize) {
                        union.push(meet);
                    }
                }
                return match union.len() {
                    0 => Simple(Empty as usize),
                    1 => union.pop().unwrap(),
                    _ => Union(union),
                };
            }
            (Union(ua), t) => {
                let mut meet = Simple(Empty as usize);
                for tu in ua {
                    meet = self.__meet(tu, t);
                    if meet != Simple(Empty as usize) {
                        break;
                    }
                }
                return meet;
            }
            (t, Union(ub)) => {
                let mut meet = Simple(Empty as usize);
                for tu in ub {
                    meet = self.__meet(t, tu);
                    if meet != Simple(Empty as usize) {
                        break;
                    }
                }
                return meet;
            }
            _ => Simple(Empty as usize),
        }
    }

    /*fn add_child(&mut self, id_type: DomainId, id_child: DomainId) {
        let childs = &mut self.childs[id_type];
        if childs[0] == Empty as usize {
            childs.remove(0);
        }
        childs.push(id_child);
    }*/

    pub fn export_dot(&self) -> String {
        let mut dot: String = "digraph {\n".to_string();

        writeln!(dot, "NONE [label = \"Empty\"]",).unwrap();

        for (id, domain) in self.domains[1..].iter().enumerate() {
            let id = id + 1;
            writeln!(dot, "{VERTICE_PREFIX}{} [label = \"{}\"]", id, domain).unwrap();

            let childs = &self.childs[id];
            if childs.is_empty() {
                writeln!(dot, "{VERTICE_PREFIX}{id} -> NONE",).unwrap();
            } else {
                for child in childs {
                    writeln!(dot, "{VERTICE_PREFIX}{id} -> {VERTICE_PREFIX}{child}",).unwrap();
                }
            }
        }

        dot.push('}');
        dot
    }
}
