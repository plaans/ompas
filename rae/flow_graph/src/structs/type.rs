use crate::structs::r#type::Type::*;
use aries_model::decomposition;
use log::Level::Debug;
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
    Vector(Box<Type>),
    Tuple(Vec<Type>),
    EmptyList,
    Handle(Box<Type>),
    Err(Box<Type>),
    //Literal,
    Symbol,
    Boolean,
    True,
    False,
    Number,
    Int,
    Float,
    New(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "Any"),
            //Literal => write!(f, "Literal"),
            Boolean => write!(f, "Boolean"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            EmptyList => write!(f, "EmptyList"),
            Number => write!(f, "Number"),
            Int => write!(f, "Int"),
            Float => write!(f, "Float"),
            List => write!(f, "List"),
            Map => write!(f, "Map"),
            Symbol => write!(f, "Symbol"),
            New(s) => write!(f, "{s}"),
            Empty => write!(f, "None"),
            Handle(h) => write!(f, "Handle[{h}]"),
            Err(e) => write!(f, "Err[{e}]"),
            Union(types) => match types.is_empty() {
                true => {
                    write!(f, "Union[Any]")
                }
                false => {
                    write!(f, "Union(")?;
                    for (i, t) in types.iter().enumerate() {
                        if i != 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, ")")
                }
            },
            Vector(t) => write!(f, "Vector[{t}]"),
            Tuple(types) => {
                write!(f, "Tuple(")?;
                for (i, t) in types.iter().enumerate() {
                    if i != 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
        }
    }
}

/*pub enum ListType {
    Any,
    Tuple(TupleType),
}*/

pub type TypeId = usize;

pub struct TypeVertice {
    r#type: Type,
    parent: Option<Type>,
    childs: HashSet<Type>,
    decomposition: Option<HashSet<Type>>,
}

impl TypeVertice {
    pub fn decomposition(&self) -> &Option<HashSet<Type>> {
        &self.decomposition
    }

    pub fn parent(&self) -> &Option<Type> {
        &self.parent
    }

    pub fn childs(&self) -> &HashSet<Type> {
        &self.childs
    }
}

pub struct TypeNetwork {
    types: Vec<TypeVertice>,
    types_ids: HashMap<Type, TypeId>,
}

const TYPE_ID_ANY: usize = 0;

impl Default for TypeNetwork {
    fn default() -> Self {
        let mut types_ids: HashMap<Type, TypeId> = Default::default();
        types_ids.insert(Any, TYPE_ID_ANY);

        let mut network = Self {
            types: vec![TypeVertice {
                r#type: Any,
                parent: None,
                childs: Default::default(),
                decomposition: None,
            }],
            types_ids,
        };
        //network.add_type(Any, None);
        network.add_type(Map, Some(Any));
        network.add_type(List, Some(Any));
        network.add_type(Boolean, Some(Any));
        network.add_type(Number, Some(Any));
        network.add_type(Symbol, Some(Any));
        network.add_type(Handle(Box::new(Any)), Some(Any));
        network.add_type(Err(Box::new(Any)), Some(Any));
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
        network.add_type(True, Some(Boolean));
        network.add_type(False, Some(Boolean));
        network.add_decomposition(&Boolean, vec![True, False].drain(..).collect());
        network.add_type(Int, Some(Number));
        network.add_type(Float, Some(Number));
        network.add_decomposition(&Number, vec![Int, Float].drain(..).collect());
        //network.add_type(Union(vec![]), vec![]);
        network.add_type(Vector(Box::new(Any)), Some(List));
        network.add_type(Tuple(vec![]), Some(List));
        network.add_type(EmptyList, Some(List));

        network
    }
}

pub type Dot = String;
const VERTICE_PREFIX: &str = "T";

impl TypeNetwork {
    //union bound

    pub fn get_type_id(&self, r#type: &Type) -> Option<TypeId> {
        self.types_ids.get(r#type).copied()
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<Type> {
        self.types.get(*type_id).map(|t| t.r#type.clone())
    }

    pub fn add_type(&mut self, r#type: Type, parent: Option<Type>) {
        let parent = parent.unwrap_or(Any);
        let id_parent = self.types_ids.get(&parent).unwrap();
        self.types[*id_parent].childs.insert(r#type.clone());
        let id = self.types.len();
        let vertice = TypeVertice {
            r#type: r#type.clone(),
            parent: Some(parent),
            childs: Default::default(),
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

    fn get_parent(&self, ta: &Type) -> Option<Type> {
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

    fn get_decomposition(&self, ta: &Type) -> Option<HashSet<Type>> {
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

    fn get_childs(&self, ta: &Type) -> HashSet<Type> {
        match ta {
            Handle(t) => self
                .get_childs(t)
                .drain()
                .map(|t| Handle(Box::new(t)))
                .collect(),
            Err(t) => self
                .get_childs(t)
                .drain()
                .map(|t| Err(Box::new(t)))
                .collect(),
            Tuple(_) => vec![EmptyList].drain(..).collect(),
            Vector(t) => self
                .get_childs(t)
                .drain()
                .map(|t| Vector(Box::new(t)))
                .collect(),
            t => {
                let id = self.get_type_id(t).unwrap();
                self.types[id].childs().iter().cloned().collect()
            }
            Empty => Default::default(),
        }
    }

    pub fn meet(&self, ta: &Type, tb: &Type) -> Type {
        if ta == tb {
            return ta.clone();
        }
        //Handling handle
        match (ta, tb) {
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

        //checking parents of ta
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
            if !vertice.childs.is_empty() {
                for child in &vertice.childs {
                    let id_child = self.types_ids.get(child).unwrap();
                    writeln!(
                        dot,
                        "{VERTICE_PREFIX}{} -> {VERTICE_PREFIX}{}",
                        id, id_child
                    )
                    .unwrap();
                }
            } else {
                writeln!(dot, "{VERTICE_PREFIX}{} -> NONE", id).unwrap();
            }
        }

        dot.push('}');
        dot
    }
}
