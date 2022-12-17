use crate::structs::chronicle::constraint::meet;
use crate::structs::domain::SimpleType::*;
use crate::structs::domain::TypeR::*;
use crate::structs::r#type::Type;
use log::Level::Debug;
use sompas_core::modules::error::check;
use std::collections::{HashMap, HashSet, VecDeque};
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeR {
    Simple(DomainId),
    Composed(DomainId, Vec<TypeR>),
    Union(Vec<TypeR>),
    Substract(Box<TypeR>, Box<TypeR>),
}

impl From<&DomainId> for TypeR {
    fn from(d: &DomainId) -> Self {
        Simple(*d)
    }
}

impl TypeR {
    pub fn from_type(dc: &DomainCollection, t: &Type) -> Self {
        match t {
            Type::Any => Simple(Any as usize),
            Type::Empty => Simple(Empty as usize),
            Type::Union(vec) => {
                let mut types: HashSet<TypeR> = Default::default();
                for t in vec {
                    match TypeR::from_type(dc, t) {
                        Union(vec) => {
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
            Type::Map => Simple(Map as usize),
            Type::List => Simple(List as usize),
            //Type::Vector(_) => {}
            //Type::Tuple(_) => {}
            Type::Err(t) => match t {
                None => Simple(Err as usize),
                Some(t) => Composed(Err as usize, vec![TypeR::from_type(dc, t)]),
            },
            Type::Handle(t) => match t {
                None => Simple(Handle as usize),
                Some(t) => Composed(Handle as usize, vec![TypeR::from_type(dc, t)]),
            },
            //Type::Alias(_, _) => {}
            Type::Symbol => Simple(Symbol as usize),
            Type::Boolean => Simple(Boolean as usize),
            Type::True => Simple(True as usize),
            Type::Nil => Simple(Nil as usize),
            Type::Number => Simple(Number as usize),
            Type::Int => Simple(Int as usize),
            Type::Float => Simple(Float as usize),
            Type::Substract(s, t) => {
                let s = TypeR::from_type(dc, s);
                let t = TypeR::from_type(dc, t);
                dc.__substract(&s, &t)
            }
            _ => unreachable!(),
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
                    Err => Type::Err(None),
                    Handle => Type::Handle(None),
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
                        Type::Err(Some(Box::new(TypeR::into_type(&sub[0]))))
                    }
                    Handle => {
                        assert_eq!(sub.len(), 1);
                        Type::Handle(Some(Box::new(TypeR::into_type(&sub[0]))))
                    }
                    _ => panic!(),
                }
            }
            Union(t) => Type::Union(t.iter().map(|t| TypeR::into_type(t)).collect()),
            Substract(t1, t2) => Type::Substract(
                Box::new(TypeR::into_type(t1)),
                Box::new(TypeR::into_type(t2)),
            ),
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
            Substract(t1, t2) => {
                format!("({} / {})", t1.format(dc), t2.format(dc))
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
        dc.add_decomposition(Boolean as usize, vec![True as usize, Nil as usize]);
        dc.add_type(Nil, vec![List as usize, Boolean as usize]);
        dc.add_type(Map, vec![]);
        dc.add_type(Err, vec![]);
        dc.add_type(Handle, vec![]);
        dc.add_type(Number, vec![]);
        dc.add_type(Int, vec![Number as usize]);
        dc.add_type(Float, vec![Number as usize]);
        dc.add_decomposition(Number as usize, vec![Int as usize, Float as usize]);
        dc.add_type(Symbol, vec![]);
        dc.add_decomposition(
            Any as usize,
            vec![
                List as usize,
                Map as usize,
                Boolean as usize,
                Handle as usize,
                Err as usize,
                Symbol as usize,
                Number as usize,
            ],
        );
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

    fn add_decomposition(&mut self, id_type: DomainId, decomposition: Vec<DomainId>) {
        self.decomposition[id_type] = decomposition;
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

    pub fn get_all_parents(&self, id: &DomainId) -> Vec<DomainId> {
        let mut queue = VecDeque::default();
        queue.push_front(*id);
        let mut parents = vec![];

        while let Some(id) = queue.pop_front() {
            if !parents.contains(&id) {
                parents.push(id);
                for id in &self.parents[id] {
                    queue.push_back(*id);
                }
            }
        }

        parents
    }

    pub fn get_decomposition(&self, id: &DomainId) -> &Vec<DomainId> {
        &self.decomposition[*id]
    }

    pub fn meet(&self, ta: &Type, tb: &Type) -> Type {
        let ta = TypeR::from_type(&self, &ta);
        let tb = TypeR::from_type(&self, &tb);

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
                    t1.clone()
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
            (Substract(t1, t2), t3) => self.__substract(&self.__meet(t1, t3), &self.__meet(t2, t3)),
            (t1, Substract(t2, t3)) => self.__substract(&self.__meet(t1, t2), &self.__meet(t1, t3)),
            _ => Simple(Empty as usize),
        }
    }

    pub fn union(&self, ta: &Type, tb: &Type) -> Type {
        let ta = TypeR::from_type(&self, &ta);
        let tb = TypeR::from_type(&self, &tb);

        TypeR::into_type(&self.__union(&ta, &tb))
    }

    fn __union(&self, t1: &TypeR, t2: &TypeR) -> TypeR {
        match (t1, t2) {
            (Simple(t1), Simple(t2)) => {
                if t1 == t2 {
                    Simple(*t1)
                } else {
                    let parents_t1 = self.get_all_parents(t1);
                    if parents_t1.contains(t2) {
                        return Simple(*t2);
                    }
                    let parents_t2 = self.get_all_parents(t2);
                    if parents_t2.contains(t1) {
                        return Simple(*t1);
                    }

                    return self.simplify_union(vec![t1.into(), t2.into()].drain(..).collect());
                }
            }
            (Simple(_), Composed(top2, _)) => {
                let nt2 = Simple(*top2);
                self.__union(t1, &nt2)
            }
            (Composed(top1, _), Simple(_)) => {
                let nt1 = Simple(*top1);
                self.__union(&nt1, t2)
            }
            (Composed(top1, comp1), Composed(top2, comp2)) => {
                if top1 == top2 && comp1.len() == comp2.len() {
                    let mut comp = vec![];
                    for (t1, t2) in comp1.iter().zip(comp2) {
                        let meet = self.__union(t1, t2);
                        comp.push(meet)
                    }
                    Composed(top1.clone(), comp)
                } else {
                    Union(vec![t1.clone(), t2.clone()])
                }
            }
            (Union(ua), Union(ub)) => {
                let mut types = ua.clone();
                types.append(&mut ub.clone());
                let mut types: HashSet<TypeR> = types.drain(..).collect();
                for ta in ua {
                    for tb in ub {
                        match self.__union(ta, tb) {
                            Union(_) => {}
                            t => {
                                types.remove(ta);
                                types.remove(tb);
                                types.insert(t);
                            }
                        }
                    }
                }
                self.simplify_union(types)
            }
            (Union(ua), tb) => {
                let mut types = ua.clone();
                types.push(tb.clone());
                let mut types: HashSet<TypeR> = types.drain(..).collect();
                for ta in ua {
                    match self.__union(ta, tb) {
                        Union(_) => {}
                        t => {
                            types.remove(ta);
                            types.remove(tb);
                            types.insert(t);
                        }
                    }
                }
                self.simplify_union(types)
            }
            (ta, Union(ub)) => {
                let mut types = ub.clone();
                types.push(ta.clone());
                let mut types: HashSet<TypeR> = types.drain(..).collect();
                for tb in ub {
                    match self.__union(ta, tb) {
                        Union(_) => {}
                        t => {
                            types.remove(ta);
                            types.remove(tb);
                            types.insert(t);
                        }
                    }
                }
                self.simplify_union(types)
            }
            (Substract(t1, t2), t3) => {
                /*println!(
                    "debug: ({}/{})|{} = ({}|{})/({}/({}^{}))",
                    t1.format(&self),
                    t2.format(&self),
                    t3.format(&self),
                    t1.format(&self),
                    t3.format(&self),
                    t2.format(&self),
                    t2.format(&self),
                    t3.format(&self)
                );*/
                self.__substract(
                    &self.__union(t1.deref(), t3),
                    &self.__substract(&t2.deref(), &self.__meet(&t2.deref(), t3)),
                )
            }
            (t1, Substract(t2, t3)) => {
                /*println!(
                    "debug: {}|({}/{})| = ({}|{})/({}/({}^{}))",
                    t1.format(&self),
                    t2.format(&self),
                    t3.format(&self),
                    t1.format(&self),
                    t2.format(&self),
                    t3.format(&self),
                    t3.format(&self),
                    t1.format(&self)
                );*/
                self.__substract(
                    &self.__union(t1, t2.deref()),
                    &self.__substract(&t3.deref(), &self.__meet(t3.deref(), t1)),
                )
            }
            _ => Simple(Empty as usize),
        }
    }

    pub fn substract(&self, ta: &Type, tb: &Type) -> Type {
        let ta = TypeR::from_type(&self, ta);
        let tb = TypeR::from_type(&self, tb);

        TypeR::into_type(&self.__substract(&ta, &tb))
    }

    fn __substract(&self, ta: &TypeR, tb: &TypeR) -> TypeR {
        let meet = self.__meet(ta, tb);
        if meet == Simple(Empty as usize) {
            ta.clone()
        } else if &meet == ta {
            Simple(Empty as usize)
        } else {
            match (ta, &meet) {
                (Union(ta), Union(tb)) => {
                    let mut ta: HashSet<TypeR> = ta.iter().cloned().collect();
                    let mut r: Vec<TypeR> = vec![];
                    for t in tb {
                        if !ta.remove(t) {
                            r.push(t.clone())
                        }
                    }
                    if ta.is_empty() {
                        return Simple(Empty as usize);
                    }
                    let mut ta: Vec<TypeR> = ta.drain().collect();
                    let t = match ta.len() {
                        0 => return Simple(Empty as usize),
                        1 => ta.pop().unwrap(),
                        _ => Union(ta),
                    };

                    match r.len() {
                        0 => t,
                        1 => Substract(Box::new(t), Box::new(r.pop().unwrap())),
                        _ => Substract(Box::new(t), Box::new(Union(r))),
                    }
                }
                (Union(ta), t2) => {
                    let mut ta: HashSet<TypeR> = ta.iter().cloned().collect();
                    let removed = ta.remove(t2);
                    let mut ta: Vec<TypeR> = ta.drain().collect();
                    let t = match ta.len() {
                        0 => return Simple(Empty as usize),
                        1 => ta.pop().unwrap(),
                        _ => Union(ta),
                    };
                    if removed {
                        t
                    } else {
                        Substract(Box::new(t), Box::new(t2.clone()))
                    }
                }
                (t1, Union(tb)) => {
                    let mut tb: HashSet<TypeR> = tb.iter().cloned().collect();
                    let removed = tb.remove(t1);
                    let mut tb: Vec<TypeR> = tb.drain().collect();
                    let t = match tb.len() {
                        0 => return Simple(Empty as usize),
                        1 => tb.pop().unwrap(),
                        _ => Union(tb),
                    };
                    if removed {
                        t
                    } else {
                        Substract(Box::new(t), Box::new(t1.clone()))
                    }
                }
                (Substract(t1, t2), _) => {
                    /*println!(
                        "debug: {}/{} = {}/({}|{})",
                        ta.format(&self),
                        tb.format(&self),
                        t1.format(&self),
                        t2.format(&self),
                        tb.format(&self)
                    );*/
                    Substract(t1.clone(), Box::new(self.__union(t2, tb)))
                }
                //(t1, Substract(t2, t3)) => Substract(Box::new(self.__union(t1, t2)), t3.clone()),
                _ => Substract(Box::new(ta.clone()), Box::new(meet)),
            }
        }
    }

    fn simplify_union(&self, mut set: HashSet<TypeR>) -> TypeR {
        let mut tested: HashSet<DomainId> = Default::default();
        let mut simples: HashSet<DomainId> = Default::default();
        let mut others: HashSet<TypeR> = Default::default();
        for t in set {
            if let Simple(t) = t {
                simples.insert(t);
            } else {
                others.insert(t);
            }
        }

        loop {
            let mut decompositions: Vec<(DomainId, &Vec<DomainId>)> = vec![];
            for t in simples.iter() {
                if *t == Any as usize {
                    return Simple(Any as usize);
                }
                if !tested.contains(t) {
                    tested.insert(t.clone());
                    let parents: &Vec<DomainId> = &self.parents[*t];
                    for parent in parents {
                        let d = self.get_decomposition(&parent);
                        if !d.is_empty() {
                            decompositions.push((*parent, d))
                        }
                    }
                }
            }
            if !decompositions.is_empty() {
                'main: for (p, decomposition) in decompositions.drain(..) {
                    for t in decomposition {
                        if !simples.contains(&t) {
                            continue 'main;
                        }
                    }
                    //If we arrive here, it means that all types of the decomposition are contained
                    for t in decomposition {
                        simples.remove(t);
                    }
                    simples.insert(p);
                }
            } else {
                break;
            }
        }

        let mut types: Vec<TypeR> = others.drain().collect();
        for s in simples {
            types.push(Simple(s))
        }
        match types.len() {
            0 => unreachable!(),
            1 => types.pop().unwrap(),
            _ => Union(types),
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
