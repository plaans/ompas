use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::root_type::RootType::*;
use crate::structs::domain::Domain::{Composed, Cst, Simple, Substract, Union};
use crate::structs::domain::{Domain, TypeId};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Write;
use std::ops::Deref;

pub struct TypeLattice {
    pub(crate) types: Vec<BasicType>,
    childs: Vec<Vec<TypeId>>,
    parents: Vec<Vec<TypeId>>,
    decomposition: Vec<Vec<TypeId>>,
    ids: HashMap<BasicType, TypeId>,
}

impl Default for TypeLattice {
    fn default() -> Self {
        let mut ids: HashMap<BasicType, TypeId> = Default::default();
        ids.insert(BasicType::RootType(Empty), 0);
        ids.insert(BasicType::RootType(Any), 0);
        let mut dc = Self {
            types: vec![BasicType::RootType(Empty), BasicType::RootType(Any)],
            childs: vec![vec![], vec![]],
            parents: vec![vec![], vec![]],
            decomposition: vec![vec![], vec![]],
            ids,
        };

        dc.add_type(Boolean, vec![]);
        dc.add_type(List, vec![]);
        dc.add_type(Map, vec![]);
        dc.add_type(Err, vec![]);
        dc.add_type(Handle, vec![]);
        dc.add_type(Number, vec![]);
        dc.add_type(Int, vec![Number as usize]);
        dc.add_type(Float, vec![Number as usize]);
        dc.add_decomposition(Number as usize, vec![Int as usize, Float as usize]);
        dc.add_type(Symbol, vec![]);
        dc.add_type(EmptyList, vec![List as usize]);
        dc.add_type(True, vec![Boolean as usize]);
        dc.add_type(False, vec![Boolean as usize]);
        dc.add_type(Nil, vec![False as usize, EmptyList as usize]);
        dc.add_decomposition(Boolean as usize, vec![True as usize, False as usize]);
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

impl TypeLattice {
    pub fn get_type_id(&self, r#type: impl Into<BasicType>) -> Option<&TypeId> {
        self.ids.get(&r#type.into())
    }

    pub fn add_type(&mut self, r#type: impl Into<BasicType>, parents: Vec<TypeId>) -> TypeId {
        let r#type = r#type.into();
        let id = self.types.len();
        self.types.push(r#type.clone());
        self.ids.insert(r#type, id);
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

    fn add_decomposition(&mut self, id_type: TypeId, decomposition: Vec<TypeId>) {
        self.decomposition[id_type] = decomposition;
    }

    fn add_parent(&mut self, id_type: TypeId, id_parent: TypeId) {
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

    pub fn get_all_childs(&self, id: &TypeId) -> Vec<TypeId> {
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

    pub fn get_all_parents(&self, id: &TypeId) -> Vec<TypeId> {
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

    pub fn get_decomposition(&self, id: &TypeId) -> &Vec<TypeId> {
        &self.decomposition[*id]
    }

    pub fn contained_in(&self, d1: &Domain, d2: &Domain) -> bool {
        match (d1, d2) {
            (Simple(t1), Simple(t2)) => {
                let childs_t2 = self.get_all_childs(t2);
                childs_t2.contains(t1)
            }
            (Simple(_), Composed(_, _)) => false,
            (Composed(top1, _), Simple(t2)) => {
                let childs_t2 = self.get_all_childs(t2);
                childs_t2.contains(top1)
            }
            (Cst(d1, c1), Cst(d2, c2)) => {
                if self.contained_in(d1, d2) {
                    c1 == c2
                } else {
                    false
                }
            }
            (Composed(top1, comp1), Composed(top2, comp2)) => {
                if top1 == top2 && comp1.len() == comp2.len() {
                    for (d1, d2) in comp1.iter().zip(comp2) {
                        if !self.contained_in(d1, d2) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            (Union(u1), Union(_)) => {
                for t1 in u1 {
                    //println!("Meet {ta} and {tb}.");
                    if !self.contained_in(t1, d2) {
                        return false;
                    }
                }
                true
            }
            (Union(_), _) => false,
            (t, Union(u2)) => {
                for t2 in u2 {
                    if self.contained_in(t, t2) {
                        return true;
                    }
                }
                false
            }
            (Substract(_, _), _) => todo!(),
            (_, Substract(_, _)) => todo!(),

            (Cst(d1, _), t2) => self.contained_in(d1, t2),
            (_, Cst(_, _)) => false,
        }
    }

    pub(crate) fn __meet(&self, t1: &Domain, t2: &Domain) -> Domain {
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
            (Union(ua), Union(_)) => {
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
                let mut meets: HashSet<Domain> = Default::default();
                let mut meet = Simple(Empty as usize);
                for tu in ua {
                    meet = self.__meet(tu, t);
                    if meet != Simple(Empty as usize) {
                        meets.insert(meet);
                    }
                }
                return self.simplify_union(meets);
            }
            (t, Union(ub)) => {
                let mut meets: HashSet<Domain> = Default::default();
                let mut meet = Simple(Empty as usize);
                for tu in ub {
                    meet = self.__meet(t, tu);
                    if meet != Simple(Empty as usize) {
                        meets.insert(meet);
                    }
                }
                return self.simplify_union(meets);
            }
            (Substract(t1, t2), t3) => self.__substract(&self.__meet(t1, t3), &self.__meet(t2, t3)),
            (t1, Substract(t2, t3)) => self.__substract(&self.__meet(t1, t2), &self.__meet(t1, t3)),
            (Cst(d1, c1), Cst(d2, c2)) => {
                let meet = self.__meet(d1, d2);
                if meet != Simple(Empty as usize) {
                    if c1 == c2 {
                        return Cst(Box::new(meet), c1.clone());
                    }
                }
                Simple(Empty as usize)
            }
            (Cst(d1, c1), t2) => {
                let meet = self.__meet(d1, t2);
                if meet != Simple(Empty as usize) {
                    Cst(Box::new(meet), c1.clone())
                } else {
                    Empty.into()
                }
            }
            (t1, Cst(d2, c2)) => {
                let meet = self.__meet(t1, d2);
                if meet != Simple(Empty as usize) {
                    return Cst(Box::new(meet), c2.clone());
                } else {
                    Empty.into()
                }
            }
        }
    }

    pub(crate) fn __union(&self, t1: &Domain, t2: &Domain) -> Domain {
        if t1.is_any() || t2.is_any() {
            Domain::any()
        } else if t1.is_empty() {
            t2.clone()
        } else if t2.is_empty() {
            t1.clone()
        } else {
            match (t1, t2) {
                (Cst(d1, c1), Cst(d2, c2)) => {
                    if c1 == c2 {
                        Cst(Box::new(self.__union(d1, d2)), c1.clone())
                    } else {
                        Union(vec![t1.clone(), t2.clone()])
                    }
                }
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

                        return self
                            .simplify_union(vec![Simple(*t1), Simple(*t2)].drain(..).collect());
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
                    let mut types: HashSet<Domain> = types.drain(..).collect();
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
                    let mut types: HashSet<Domain> = types.drain(..).collect();
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
                    let mut types: HashSet<Domain> = types.drain(..).collect();
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

                (Cst(d1, _), t2) => {
                    let union = self.__union(d1, t1);

                    if &union == t2 {
                        t2.clone()
                    } else {
                        Union(vec![t1.clone(), t2.clone()])
                    }
                }
                (t1, Cst(d2, _)) => {
                    let union = self.__union(t1, d2);

                    if &union == t1 {
                        t1.clone()
                    } else {
                        Union(vec![t1.clone(), t2.clone()])
                    }
                }
            }
        }
    }

    pub(crate) fn __substract(&self, t1: &Domain, t2: &Domain) -> Domain {
        let meet = self.__meet(t1, t2);
        if meet == Simple(Empty as usize) {
            t1.clone()
        } else if &meet == t1 {
            Simple(Empty as usize)
        } else {
            match (t1, &meet) {
                (Union(ta), Union(tb)) => {
                    let mut ta: HashSet<Domain> = ta.iter().cloned().collect();
                    let mut r: Vec<Domain> = vec![];
                    for t in tb {
                        if !ta.remove(t) {
                            r.push(t.clone())
                        }
                    }
                    if ta.is_empty() {
                        return Simple(Empty as usize);
                    }
                    let mut ta: Vec<Domain> = ta.drain().collect();
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
                    let mut ta: HashSet<Domain> = ta.iter().cloned().collect();
                    let removed = ta.remove(t2);
                    let mut ta: Vec<Domain> = ta.drain().collect();
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
                    let mut tb: HashSet<Domain> = tb.iter().cloned().collect();
                    let removed = tb.remove(t1);
                    let mut tb: Vec<Domain> = tb.drain().collect();
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
                (Substract(t1, t2), t3) => {
                    /*println!(
                        "debug: {}/{} = {}/({}|{})",
                        ta.format(&self),
                        tb.format(&self),
                        t1.format(&self),
                        t2.format(&self),
                        tb.format(&self)
                    );*/
                    Substract(t1.clone(), Box::new(self.__union(t2, t3)))
                }
                (Cst(d1, c1), Cst(d2, c2)) => {
                    if c1 == c2 {
                        let sub = self.__substract(d1, d2);
                        if sub == Empty.into() {
                            Empty.into()
                        } else {
                            Cst(Box::new(sub), c1.clone())
                        }
                    } else {
                        Substract(Box::new(t1.clone()), Box::new(t2.clone()))
                    }
                }
                //(t1, Substract(t2, t3)) => Substract(Box::new(self.__union(t1, t2)), t3.clone()),
                _ => Substract(Box::new(t1.clone()), Box::new(meet)),
            }
        }
    }

    pub(crate) fn simplify_union(&self, set: HashSet<Domain>) -> Domain {
        let mut tested: HashSet<TypeId> = Default::default();
        let mut simples: HashSet<TypeId> = Default::default();
        let mut others: HashSet<Domain> = Default::default();
        for t in set {
            if let Simple(t) = t {
                simples.insert(t);
            } else {
                others.insert(t);
            }
        }

        loop {
            let mut decompositions: Vec<(TypeId, &Vec<TypeId>)> = vec![];
            for t in simples.iter() {
                if *t == Any as usize {
                    return Simple(Any as usize);
                }
                if !tested.contains(t) {
                    tested.insert(t.clone());
                    let parents: &Vec<TypeId> = &self.parents[*t];
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

        let mut types: Vec<Domain> = others.drain().collect();
        for s in simples {
            types.push(Simple(s))
        }
        match types.len() {
            0 => Domain::empty(),
            1 => types.pop().unwrap(),
            _ => Union(types),
        }
    }

    /*fn add_child(&mut self, id_type: TypeId, id_child: TypeId) {
        let childs = &mut self.childs[id_type];
        if childs[0] == Empty as usize {
            childs.remove(0);
        }
        childs.push(id_child);
    }*/

    pub fn export_dot(&self) -> String {
        let mut dot: String = "digraph {\n".to_string();

        writeln!(dot, "NONE [label = \"Empty\"]",).unwrap();

        for (id, domain) in self.types[1..].iter().enumerate() {
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

const VERTICE_PREFIX: &str = "D";
