use crate::structs::r#type::Type::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    List,
    Map,
    TypedList(Box<Type>),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Handle(Box<Type>),
    Err(Box<Type>),
    Literal,
    Symbol,
    None,
    Boolean,
    True,
    Nil,
    Number,
    Int,
    Float,
    New(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Any => write!(f, "Any"),
            Literal => write!(f, "Literal"),
            Boolean => write!(f, "Boolean"),
            True => write!(f, "True"),
            Nil => write!(f, "Nil"),
            Number => write!(f, "Number"),
            Int => write!(f, "Int"),
            Float => write!(f, "Float"),
            List => write!(f, "List"),
            Map => write!(f, "Map"),
            Symbol => write!(f, "Symbol"),
            New(s) => write!(f, "{s}"),
            None => write!(f, "None"),
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
            TypedList(t) => write!(f, "TypedList[{t}]"),
            Tuple(types) => match types.is_empty() {
                true => {
                    write!(f, "Tuple[Any]")
                }
                false => {
                    write!(f, "Tuple(")?;
                    for (i, t) in types.iter().enumerate() {
                        if i != 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{}", t)?;
                    }
                    write!(f, ")")
                }
            },
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
    parents: HashSet<TypeId>,
    childs: HashSet<TypeId>,
}

impl TypeVertice {
    pub fn childs(&self) -> Vec<TypeId> {
        self.childs.iter().copied().collect()
    }

    pub fn parents(&self) -> Vec<TypeId> {
        self.parents.iter().copied().collect()
    }
}

pub struct TypeNetwork {
    types: Vec<TypeVertice>,
    types_ids: HashMap<Type, TypeId>,
}

const TYPE_ID_ANY: usize = 0;
const TYPE_ID_NONE: usize = 1;

impl Default for TypeNetwork {
    fn default() -> Self {
        let mut types_ids: HashMap<Type, TypeId> = Default::default();
        types_ids.insert(Any, TYPE_ID_ANY);
        types_ids.insert(None, TYPE_ID_NONE);

        let mut network = Self {
            types: vec![
                TypeVertice {
                    r#type: Any,
                    parents: Default::default(),
                    childs: [TYPE_ID_NONE].into(),
                },
                TypeVertice {
                    r#type: None,
                    parents: [TYPE_ID_ANY].into(),
                    childs: Default::default(),
                },
            ],
            types_ids,
        };
        assert_eq!(network.get_type_id(&Any).unwrap(), 0);
        assert_eq!(network.get_type_id(&None).unwrap(), 1);
        network.add_type(Map, vec![]);
        network.add_type(List, vec![]);
        network.add_type(Literal, vec![]);
        network.add_type(Boolean, vec![Literal]);
        network.add_type(True, vec![Boolean]);
        network.add_type(Nil, vec![Boolean, List]);
        network.add_type(Symbol, vec![Literal]);
        network.add_type(Number, vec![Literal]);
        network.add_type(Int, vec![Number]);
        network.add_type(Float, vec![Number]);
        network.add_type(Handle(Box::new(Any)), vec![]);
        network.add_type(Err(Box::new(Any)), vec![]);
        //network.add_type(Union(vec![]), vec![]);
        network.add_type(TypedList(Box::new(Any)), vec![List]);
        network.add_type(Tuple(vec![]), vec![List, TypedList(Box::new(Any))]);

        network
    }
}

pub type Dot = String;
const VERTICE_PREFIX: &str = "T";

impl TypeNetwork {
    //union bound

    fn get_parents(&self, ta: &Type) -> Vec<Type> {
        match ta {
            Handle(t) => match t.deref() {
                Any => vec![Any],
                t => self
                    .get_parents(t)
                    .drain(..)
                    .map(|t| Handle(Box::new(t)))
                    .collect(),
            },
            Err(t) => match t.deref() {
                Any => vec![Any],
                t => self
                    .get_parents(t)
                    .drain(..)
                    .map(|t| Err(Box::new(t)))
                    .collect(),
            },
            t => {
                let id = self.get_type_id(t).unwrap();
                self.types[id]
                    .parents()
                    .drain(..)
                    .map(|id| self.get_type(&id).unwrap())
                    .collect()
            }
        }
    }

    fn get_childs(&self, ta: &Type) -> Vec<Type> {
        match ta {
            Handle(t) => self
                .get_childs(t.deref())
                .drain(..)
                .map(|t| Handle(Box::new(t)))
                .collect(),
            Err(t) => self
                .get_childs(t)
                .drain(..)
                .map(|t| Err(Box::new(t)))
                .collect(),
            t => {
                let id = self.get_type_id(t).unwrap();
                self.types[id]
                    .childs()
                    .drain(..)
                    .map(|id| self.get_type(&id).unwrap())
                    .collect()
            }
        }
    }

    fn union_set(&self, set: HashSet<Type>) -> Type {
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
                if t != None {
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
            0 => None,
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
            (TypedList(ea), TypedList(eb)) => return TypedList(Box::new(self.union(&ea, &eb))),
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
                let types: HashSet<Type> = types.drain(..).collect();
                return self.union_set(types);
            }
            (Union(ua), t) => {
                let mut types: HashSet<Type> = ua.clone().drain(..).collect();
                types.insert(t.clone());
                return self.union_set(types);
            }
            (t, Union(ub)) => {
                let mut types: HashSet<Type> = ub.clone().drain(..).collect();
                types.insert(t.clone());
                return self.union_set(types);
            }
            _ => {}
        }

        let type_id_a = self.get_type_id(ta).unwrap();
        let type_id_b = self.get_type_id(tb).unwrap();
        //checking childs
        let mut queue: Vec<TypeId> = self.types[type_id_a].childs();
        while let Some(t) = queue.pop() {
            if t != TYPE_ID_NONE {
                if type_id_b == t {
                    return ta.clone();
                } else {
                    queue.append(&mut self.types[t].childs())
                }
            }
        }

        //checking parents
        let mut queue: Vec<TypeId> = self.types[type_id_a].parents();
        while let Some(t) = queue.pop() {
            if type_id_b == t {
                return tb.clone();
            } else {
                queue.append(&mut self.types[t].parents())
            }
        }

        self.try_merge(vec![ta.clone(), tb.clone()].drain(..).collect())
    }

    pub fn meet(&self, ta: &Type, tb: &Type) -> Type {
        if ta == tb {
            return ta.clone();
        }
        //Handling handle
        match (ta, tb) {
            (Any, Handle(_) | Err(_) | TypedList(_) | Tuple(_) | Union(_)) => return tb.clone(),
            (Handle(_) | Err(_) | TypedList(_) | Tuple(_) | Union(_), Any) => return ta.clone(),
            //handle case
            (Handle(ha), Handle(hb)) => {
                let t = self.meet(&ha, &hb);
                return if t != None { Handle(Box::new(t)) } else { None };
            }
            //err case
            (Err(ea), Err(eb)) => {
                let t = self.meet(&ea, &eb);
                return if t != None { Err(Box::new(t)) } else { None };
            }
            //typed list case
            (TypedList(ea), TypedList(eb)) => {
                let t = self.meet(&ea, &eb);
                return if t != None {
                    TypedList(Box::new(t))
                } else {
                    None
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
                            None => return None,
                            t => vec.push(t),
                        }
                    }
                    Tuple(vec)
                } else {
                    None
                };
            }
            (Union(ua), Union(ub)) => {
                return self.meet_sets(ua.iter().cloned().collect(), ub.iter().cloned().collect());
            }
            (Err(_) | Handle(_) | TypedList(_) | Tuple(_) | Union(_), _)
            | (_, Err(_) | Handle(_) | TypedList(_) | Tuple(_) | Union(_)) => return None,
            _ => {}
        };

        let type_id_a = self.get_type_id(ta).unwrap();
        let type_id_b = self.get_type_id(tb).unwrap();
        //checking childs
        let mut queue: Vec<TypeId> = self.types[type_id_a].childs();
        while let Some(t) = queue.pop() {
            if t != TYPE_ID_NONE {
                if type_id_b == t {
                    return tb.clone();
                } else {
                    queue.append(&mut self.types[t].childs())
                }
            }
        }

        //checking parents
        let mut queue: Vec<TypeId> = self.types[type_id_a].parents();
        while let Some(t) = queue.pop() {
            if type_id_b == t {
                return ta.clone();
            } else {
                queue.append(&mut self.types[t].parents())
            }
        }

        return None;
    }

    pub fn get_type_id(&self, r#type: &Type) -> Option<TypeId> {
        self.types_ids.get(r#type).copied()
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<Type> {
        self.types.get(*type_id).map(|t| t.r#type.clone())
    }

    pub fn add_type(&mut self, r#type: Type, parents: Vec<Type>) {
        let id = self.types.len();
        let vertice = TypeVertice {
            r#type: r#type.clone(),
            parents: Default::default(),
            childs: Default::default(),
        };
        self.types.push(vertice);
        self.types_ids.insert(r#type, id);
        self.add_parent(TYPE_ID_ANY, id);
        self.add_parent(id, TYPE_ID_NONE);

        for parent in &parents {
            let parent_id = self.get_type_id(parent).unwrap();
            self.add_parent(parent_id, id);
        }
    }

    fn add_parent(&mut self, parent: TypeId, child: TypeId) {
        //Cleaning Any and None types;
        self.types[TYPE_ID_ANY].childs.remove(&child);

        let childs = &mut self.types[parent].childs;
        childs.remove(&TYPE_ID_NONE);
        childs.insert(child);
        let parents = &mut self.types[child].parents;
        parents.remove(&TYPE_ID_ANY);
        parents.insert(parent);
    }

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        for (id, vertice) in self.types.iter().enumerate() {
            writeln!(
                dot,
                "{VERTICE_PREFIX}{} [label = \"{}\"]",
                id, vertice.r#type
            )
            .unwrap();
            for child in &vertice.childs {
                writeln!(dot, "{VERTICE_PREFIX}{} -> {VERTICE_PREFIX}{}", id, child).unwrap();
            }
        }

        dot.push('}');
        dot
    }
}
