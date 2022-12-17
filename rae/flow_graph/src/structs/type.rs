use crate::structs::r#type::Type::*;
use aries_model::decomposition;
use log::Level::Debug;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Empty,
    Union(Vec<Type>),
    Map,
    List,
    Vector(Option<Box<Type>>),
    Tuple(Option<Vec<Type>>),
    //EmptyList,
    Handle(Option<Box<Type>>),
    Err(Option<Box<Type>>),
    Alias(Box<Type>, Box<Type>),
    //Literal,
    Symbol,
    Boolean,
    True,
    Nil,
    Number,
    Int,
    Float,
    New(String),
    Substract(Box<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "Any"),
            Boolean => write!(f, "Boolean"),
            True => write!(f, "True"),
            Number => write!(f, "Number"),
            Int => write!(f, "Int"),
            Float => write!(f, "Float"),
            List => write!(f, "List"),
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
            Alias(n, t) => write!(f, "{n}"),
            Nil => write!(f, "Nil"),
            Substract(t1, t2) => write!(f, "({t1} / {t2})"),
        }
    }
}

/*
#[derive(Copy, Clone)]
pub enum TypeGeneric {
    Type(TypeId),
    Union(Vec<TypeId>),
    Err(TypeId),
    Handle(TypeId),
    Vec(TypeId),
    Tuple(Vec<TypeId>),
}

pub enum DomainKind {
    Cst(LValue),
    Type(Type),
}

impl From<Type> for DomainKind {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        New(s.to_string())
    }
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

/*#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Cst {
    r#type: Type,
    value: LValue,
}*/


/*pub enum ListType {
    Any,
    Tuple(TupleType),
}*/

pub type TypeId = usize;

pub struct Domain {
    r#type: Type,
    parents: Vec<TypeId>,
    childs: Vec<TypeId>,
    decomposition: Option<Vec<TypeId>>,
}

impl Domain {
    pub fn decomposition(&self) -> &Option<Vec<TypeId>> {
        &self.decomposition
    }

    pub fn parents(&self) -> &Vec<TypeId> {
        &self.parents
    }

    pub fn childs(&self) -> &Vec<TypeId> {
        &self.childs
    }
}

pub struct TypeNetwork {
    types: Vec<Domain>,
    types_ids: HashMap<Type, TypeId>,
}

const TYPE_ID_ANY: usize = 0;
const TYPE_ID_EMPTY: usize = 1;

impl Default for TypeNetwork {
    fn default() -> Self {
        let mut types_ids: HashMap<Type, TypeId> = Default::default();
        types_ids.insert(Any, TYPE_ID_ANY);
        types_ids.insert(Empty, TYPE_ID_EMPTY);
        let mut network = Self {
            types: vec![Domain {
                r#type: Any.into(),
                parents: vec![],
                childs: vec![TYPE_ID_EMPTY],
                decomposition: None,
            }],
            types_ids,
        };

        //network.add_type(Any, None);
        network.add_type(Map, vec![Any]);
        network.add_type(List, vec![Any]);
        network.add_type(Boolean, vec![Any]);
        network.add_type(Number, vec![Any]);
        network.add_type(Symbol, vec![Any]);
        network.add_type(Handle(Box::new(Any)), vec![Any]);
        network.add_type(Err(Box::new(Any)), vec![Any]);
        network.add_decomposition(
            &Any,
            vec![
                Map,
                List,
                Boolean,
                Number,
                Symbol,
                Handle(Box::new(Any)),
                Err(Box::new(Any)),
            ]
            .drain(..)
            .collect(),
        );
        /*network.add_type(True, Some(Boolean));
        network.add_type(False, Some(Boolean));
        network.add_decomposition(&Boolean, vec![True, False].drain(..).collect());
        network.add_type(Int, Some(Number));
        network.add_type(Float, Some(Number));
        network.add_alias("Timepoint", Number);
        network.add_decomposition(&Number, vec![Int, Float].drain(..).collect());
        //network.add_type(Union(vec![]), vec![]);
        network.add_type(Vector(Box::new(Any)), Some(List));
        network.add_type(Tuple(vec![]), Some(List));
        network.add_type(EmptyList, Some(List));*/

        network
    }
}

pub type Dot = String;
const VERTICE_PREFIX: &str = "T";

impl TypeNetwork {
    /*pub fn add_const(&self, lv: LValue, r#type: Option<Type>) -> Result<(), ()> {
        //check valeur du type

        let cst = match r#type {
            None => match &lv {
                LValue::Symbol(s) => Cst {
                    r#type: Box::new(Symbol),
                    value: lv,
                },
                LValue::String(_) => {}
                LValue::Number(n) => match n {
                    LNumber::Int(_) => Cst {
                        r#type: Box::new(Int),
                        value: lv,
                    },
                    LNumber::Float(_) => Cst {
                        r#type: Box::new(Float),
                        value: lv,
                    },
                },
                LValue::Fn(_) => {}
                LValue::MutFn(_) => {}
                LValue::AsyncFn(_) => {}
                LValue::AsyncMutFn(_) => {}
                LValue::Lambda(_) => {}
                LValue::Primitive(_) => {}
                LValue::Handle(_) => {}
                LValue::Err(_) => {}
                LValue::Map(_) => {}
                LValue::List(_) => {}
                LValue::True => Cst {
                    r#type: Box::new(True),
                    value: lv,
                },
                LValue::Nil => Cst {
                    r#type: Box::new(Union(vec![Boolean, EmptyList])),
                    value: lv,
                },
            },
            Some(t) => {
                let mut tcst = t;
                while let New(_) = &tcst {
                    tcst = self.get_parent(&tcst).unwrap()
                }
                match tcst {
                    Any => Cst {
                        r#type: Box::new(Any),
                        value: lv,
                    },
                    Union(_) => {}
                    Map => {
                        if let LValue::Map(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        } else {
                            return Err(());
                        }
                    }
                    List => {
                        if let LValue::List(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        } else {
                            return Err(());
                        }
                    }
                    Vector(_) => {}
                    Tuple(_) => {}
                    EmptyList => {
                        if let LValue::Nil = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        }
                    }
                    Handle(_) => {}
                    Err(_) => {
                        if let LValue::Map(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        } else {
                            Err(())
                        }
                    }
                    Symbol => {
                        if let LValue::Map(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        } else {
                            Err(())
                        }
                    }
                    Boolean => {}
                    True => {
                        if let LValue::True(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        }else {
                            Err(())
                        }
                    }
                    False => {
                        if let LValue::True(_) = &lv {
                            Cst {
                                r#type: Box::new(Int),
                                value: lv,
                            }
                        }else {
                            Err(())
                        }
                    }
                    Number => {}
                    Int => {}
                    Float => {}
                    New(_) | Cst(_) | Empty => unreachable!(),
                }
            }
        };
    }*/

    pub fn get_type_id(&self, r#type: &Type) -> Option<TypeId> {
        self.types_ids.get(r#type).copied()
    }

    /*pub fn add_alias(&mut self, alias: impl ToString, t: Type) {
        let id = self.types.len();
        let vertice = Domain {
            r#type: Alias(Box::new(New(alias.to_string())), Box::new(t)),
            parent: None,
            childs: Default::default(),
            decomposition: None,
        };
        self.types.push(vertice);
        self.types_ids.insert(New(alias.to_string()), id);
    }*/

    pub fn add_type(&mut self, r#type: impl Into<Type>, mut parents: Vec<impl Into<Type>>) {
        let id = self.types.len();
        let r#type = r#type.into();
        let parents = match parents.is_empty() {
            true => vec![TYPE_ID_ANY],
            false => parents
                .drain(..)
                .map(|t| self.get_type_id(&t.into()).unwrap()),
        };
        for p in parents {
            self.types[p].childs.push(id)
        }
        let vertice = Domain {
            r#type: r#type.clone(),
            parents,
            childs: vec![TYPE_ID_EMPTY],
            decomposition: None,
        };
        self.types.push(vertice);
        self.types_ids.insert(r#type, id);
    }

    fn add_decomposition(&mut self, parent: &Type, decomposition: HashSet<Type>) {
        let id = self.get_type_id(parent).unwrap();
        for t in decomposition.iter() {
            assert!(self.types_ids.contains_key(t))
        }
        self.types[id].decomposition = Some(decomposition);
    }

    fn get_parents(&self, ta: &TypeId) -> Option<Type> {
        match ta {
            Any => None,
            Handle(t) => match t.deref() {
                Any => Some(Any),
                t => self.get_parent(t).map(|t| Handle(Box::new(t))),
            },
            Tuple(_) => Some(List),
            Vector(t) => match t.deref() {
                Any => Some(List),
                t => self.get_parent(t).map(|t| Vector(Box::new(t))),
            },

            Err(t) => match t.deref() {
                Any => Some(Any),
                t => self.get_parent(t).map(|t| Err(Box::new(t))),
            },

            t => {
                let id = self.get_type_id(t).unwrap();
                self.types[id].parent().clone()
            }
        }
    }

    fn get_decomposition(&self, ta: &Type) -> Option<Vec<TypeId>> {
        match ta {
            Err(t) => self.get_decomposition(t).map(|decomposition| {
                decomposition
                    .iter()
                    .map(|t| Err(Box::new(t.clone())))
                    .collect()
            }),
            Handle(t) => self.get_decomposition(t).map(|decomposition| {
                decomposition
                    .iter()
                    .map(|t| Handle(Box::new(t.clone())))
                    .collect()
            }),
            Vector(t) => self.get_decomposition(t).map(|decomposition| {
                decomposition
                    .iter()
                    .map(|t| Handle(Box::new(t.clone())))
                    .collect()
            }),
            Union(_) => None,
            t => {
                let id = self.get_type_id(t).unwrap();
                self.types[id].decomposition.clone()
            }
        }
    }

    fn get_childs(&self, id: &TypeId) -> Vec<TypeId> {
        self.types[*id].childs().clone()
        /*match ta {
            /*Handle(t) => self
                .get_childs(t)
                .drain()
                .map(|t| Handle(Box::new(t)))
                .collect(),
            Err(t) => self
                .get_childs(t)
                .drain()
                .map(|t| Err(Box::new(t)))
                .collect(),*/
            //Tuple(_) => vec![EmptyList].drain(..).collect(),
            Vector(t) | Handle(_) | Err(_) => unreachable!(),
            t => {
                self.types[id].childs().iter().cloned().collect()
            }
            Empty => Default::default(),
        }*/
    }

    pub fn meet(&self, ta: &Type, tb: &Type) -> Type {
        let id_a = &self.types_ids[ta];
        let id_b = &self.types_ids[tb];

        self.types[self.meet_id(id_a, id_b)].r#type.clone()
    }

    fn meet_id(&self, ta: &TypeId, tb: &TypeId) -> TypeId {
        if ta == tb {
            return *ta;
        }
        //Handling handle
        match (ta, tb) {
            /*(Alias(t1, t2), tb) => {
                let r = self.meet(t2, tb);
                return if &r == t2.deref() {
                    t1.deref().clone()
                } else {
                    r
                };
            }
            (ta, Alias(t1, t2)) => {
                let r = self.meet(ta, t2);
                return if &r == t2.deref() {
                    t1.deref().clone()
                } else {
                    r
                };
            }*/
            (Any, Handle(_) | Err(_) | Vector(_) | Tuple(_) | Union(_)) => return tb.clone(),
            (Handle(_) | Err(_) | Vector(_) | Tuple(_) | Union(_), Any) => return ta.clone(),
            //handle case
            (Handle(ha), Handle(hb)) => {
                let t = self.meet(&ha, &hb);
                return if t != Empty {
                    Handle(Box::new(t))
                } else {
                    Empty
                };
            }
            //err case
            (Err(ea), Err(eb)) => {
                let t = self.meet(&ea, &eb);
                return if t != Empty { Err(Box::new(t)) } else { Empty };
            }
            //typed list case
            (Vector(va), Vector(vb)) => {
                let t = self.meet(&va, &vb);
                return if t != Empty {
                    Vector(Box::new(t))
                } else {
                    Empty
                };
            }
            //tuple case
            (Tuple(veca), Tuple(vecb)) => {
                if veca.is_empty() {
                    return tb.clone();
                }
                if vecb.is_empty() {
                    return ta.clone();
                }
                return if veca.len() == vecb.len() {
                    let mut vec = vec![];

                    for (t1, t2) in veca.iter().zip(vecb) {
                        match self.meet(t1, t2) {
                            Empty => return Empty,
                            t => vec.push(t),
                        }
                    }
                    Tuple(vec)
                } else {
                    Empty
                };
            }
            (Union(ua), Union(_)) => {
                //println!("Meet {ta} and {tb}.");
                let mut union = vec![];
                for ta in ua {
                    //println!("Meet {ta} and {tb}.");
                    let meet = self.meet(ta, tb);
                    if meet != Empty {
                        union.push(meet);
                    }
                }
                return match union.len() {
                    0 => Empty,
                    1 => union.pop().unwrap(),
                    _ => Union(union),
                };
            }
            (Union(ua), t) => {
                let mut meet: Type = Empty;
                for tu in ua {
                    meet = self.meet(tu, t);
                    if meet != Empty {
                        break;
                    }
                }
                return meet;
            }
            (t, Union(ub)) => {
                let mut meet: Type = Empty;
                for tu in ub {
                    meet = self.meet(tu, t);
                    if meet != Empty {
                        break;
                    }
                }
                return meet;
            }

            (Err(_) | Handle(_) | Vector(_) | Tuple(_), _)
            | (_, Err(_) | Handle(_) | Vector(_) | Tuple(_)) => return Empty,
            _ => {}
        };

        //checking childs of ta
        let mut parent: Option<Type> = self.get_parent(ta);
        while let Some(p) = &parent {
            if tb == p {
                return ta.clone();
            } else {
                parent = self.get_parent(p)
            }
        }

        //checking parents of tb
        let mut parent: Option<Type> = self.get_parent(tb);
        while let Some(p) = &parent {
            if ta == p {
                return tb.clone();
            } else {
                parent = self.get_parent(p)
            }
        }

        return Empty;
    }

    pub fn union(&self, ta: &Type, tb: &Type) -> Type {
        if ta == tb {
            return ta.clone();
        } else if ta == &Any {
            return Any;
        } else if tb == &Any {
            return Any;
        }
        //Handling handle
        match (ta, tb) {
            //handle case
            (Alias(t1, t2), tb) => {
                let r = self.union(t2, tb);
                return if &r == t2.deref() {
                    t1.deref().clone()
                } else {
                    r
                };
            }
            (ta, Alias(t1, t2)) => {
                let r = self.union(ta, t2);
                return if &r == t2.deref() {
                    t1.deref().clone()
                } else {
                    r
                };
            }
            (Handle(ha), Handle(hb)) => return Handle(Box::new(self.union(&ha, &hb))),
            //Error case
            (Err(ea), Err(eb)) => return Err(Box::new(self.union(&ea, &eb))),
            //typed list
            (Vector(ea), Vector(eb)) => return Vector(Box::new(self.union(&ea, &eb))),
            //tuple case
            (Tuple(veca), Tuple(vecb)) => {
                return if veca.len() == vecb.len() {
                    Tuple(
                        veca.iter()
                            .zip(vecb)
                            .map(|(t1, t2)| self.union(t1, t2))
                            .collect(),
                    )
                } else {
                    Union(vec![ta.clone(), tb.clone()])
                }
            }
            //union cases
            (Union(ua), Union(ub)) => {
                let mut types = ua.clone();
                types.append(&mut ub.clone());
                let mut types: HashSet<Type> = types.drain(..).collect();
                for ta in ua {
                    for tb in ub {
                        match self.union(ta, tb) {
                            Union(_) => {}
                            t => {
                                types.remove(&ta);
                                types.remove(&tb);
                                types.insert(t);
                            }
                        }
                    }
                }
                //println!("debug: {ta} | {tb} => {:?}", types);
                return self.simplify_union(types);
            }
            (Union(ua), tb) => {
                let mut types: HashSet<Type> = ua.iter().cloned().collect();
                types.insert(tb.clone());
                for ta in ua {
                    match self.union(ta, tb) {
                        Union(_) => {}
                        t => {
                            types.remove(&ta);
                            types.remove(&tb);
                            types.insert(t);
                        }
                    }
                }
                println!("debug: {ta} | {tb} => {:?}", types);
                return self.simplify_union(types);
            }
            (ta, Union(ub)) => {
                let mut types: HashSet<Type> = ub.iter().cloned().collect();
                types.insert(ta.clone());
                for tb in ub {
                    match self.union(ta, tb) {
                        Union(_) => {}
                        t => {
                            types.remove(&ta);
                            types.remove(&tb);
                            types.insert(t);
                        }
                    }
                }
                println!("debug: {ta} | {tb} => {:?}", types);
                return self.simplify_union(types);
            }
            _ => {}
        }

        //checking parents of ta
        let mut parent: Option<Type> = self.get_parent(ta);
        while let Some(p) = &parent {
            if tb == p {
                return tb.clone();
            } else {
                parent = self.get_parent(p)
            }
        }

        //checking parents of tb
        let mut parent: Option<Type> = self.get_parent(tb);
        while let Some(p) = &parent {
            if ta == p {
                return ta.clone();
            } else {
                parent = self.get_parent(p)
            }
        }

        self.simplify_union(vec![ta.clone(), tb.clone()].drain(..).collect())

        //self.try_merge(vec![ta.clone(), tb.clone()].drain(..).collect())
    }

    fn simplify_union(&self, mut set: HashSet<Type>) -> Type {
        let mut tested = vec![];

        loop {
            let mut decompositions: Vec<(Type, HashSet<Type>)> = vec![];
            for t in set.iter() {
                if t == &Any {
                    return Any;
                }
                if !tested.contains(t) {
                    tested.push(t.clone());
                    let parent = self.get_parent(t).unwrap();
                    if let Some(d) = self.get_decomposition(&parent) {
                        decompositions.push((parent, d))
                    }
                }
            }
            if !decompositions.is_empty() {
                'main: for (p, decomposition) in decompositions.drain(..) {
                    for t in &decomposition {
                        if !set.contains(&t) {
                            continue 'main;
                        }
                    }
                    //If we arrive here, it means that all types of the decomposition are contained
                    for t in &decomposition {
                        set.remove(t);
                    }
                    set.insert(p);
                }
            } else {
                break;
            }
        }

        let mut types: Vec<Type> = set.drain().collect();
        match types.len() {
            0 => unreachable!(),
            1 => types.pop().unwrap(),
            _ => Union(types),
        }
    }

    /*fn union_set(&self, set: HashSet<Type>) -> Type {
        //All types
        let mut new_types: Vec<Type> = vec![];
        'main: for t in set {
            for i in 0..new_types.len() {
                match self.union(&t, &new_types[i]) {
                    Union(_) => {}
                    t => {
                        new_types.remove(i);
                        new_types.push(t);
                        continue 'main;
                    }
                }
            }
            new_types.push(t)
        }

        self.try_merge(new_types.drain(..).collect())
    }

    fn meet_sets(&self, seta: HashSet<Type>, setb: HashSet<Type>) -> Type {
        let mut set: HashSet<Type> = HashSet::default();
        for ta in seta.iter() {
            'l: for tb in setb.iter() {
                let t = self.meet(ta, tb);
                if t != Empty {
                    let mut union: Option<(Type, Type)> = Option::None;
                    for e in set.iter() {
                        match self.union(e, &t) {
                            Union(_) => {}
                            t => {
                                union = Some((e.clone(), t));
                                break;
                            }
                        }
                    }
                    match union {
                        Option::None => {
                            set.insert(t);
                        }
                        Some((t1, t2)) => {
                            set.remove(&t1);
                            set.insert(t2);
                        }
                    }
                }
            }
        }

        let mut set: Vec<Type> = set.drain().collect();
        match set.len() {
            0 => Empty,
            1 => set.pop().unwrap(),
            _ => Union(set),
        }
    }

    //Merge unique types to parent type if possible
    pub fn try_merge(&self, mut types: HashSet<Type>) -> Type {
        println!("Try Merge: {:?}", types);
        let parent_checked: HashSet<Type> = HashSet::default();
        let mut types: HashSet<Type> = types.drain().collect();

        let mut potential_parents: HashSet<Type> = HashSet::default();
        for t in types.iter() {
            let parents = self.get_parents(&t);
            println!("Parents of {t}: {:?}", parents);
            for p in parents {
                potential_parents.insert(p);
            }
        }
        println!("Potential parents: {:?}", potential_parents);

        let mut vec_potential_parents: Vec<Type> = potential_parents.iter().cloned().collect();
        let mut types: HashSet<Type> = types.drain().collect();

        'check: while let Some(p) = vec_potential_parents.pop() {
            let childs = self.get_childs(&p);
            println!("Childs of {p}: {:?}", childs);
            for child in &childs {
                if !types.contains(child) {
                    continue 'check;
                }
            }
            println!("{:?} contains all the childs of {}({:?})", types, p, childs);
            for child in &childs {
                types.remove(&child);
            }
            let parents = self.get_parents(&p);
            for p in parents {
                if !potential_parents.contains(&p) {
                    vec_potential_parents.push(p)
                }
            }
            types.insert(p);
        }

        let mut types: Vec<Type> = types.drain().collect();

        if types.len() == 1 {
            types.pop().unwrap()
        } else {
            Union(types)
        }
    }*/

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        writeln!(dot, "NONE [label = \"None\"]",).unwrap();

        for (id, vertice) in self.types.iter().enumerate() {
            writeln!(
                dot,
                "{VERTICE_PREFIX}{} [label = \"{}\"]",
                id, vertice.r#type
            )
            .unwrap();
            if let Alias(t1, t2) = &vertice.r#type {
                //println!("{t1} is alias of {t2}");
                let id_type = self.types_ids.get(t2).unwrap();
                writeln!(
                    dot,
                    "{VERTICE_PREFIX}{id} -> {VERTICE_PREFIX}{id_type}[arrowhead=\"none\"]",
                )
                .unwrap();
            } else if !vertice.childs.is_empty() {
                for child in &vertice.childs {
                    let id_child = self.types_ids.get(child).unwrap();
                    writeln!(dot, "{VERTICE_PREFIX}{id} -> {VERTICE_PREFIX}{id_child}",).unwrap();
                }
            } else {
                writeln!(dot, "{VERTICE_PREFIX}{} -> NONE", id).unwrap();
            }
        }

        dot.push('}');
        dot
    }
}*/
