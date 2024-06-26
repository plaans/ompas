use crate::model::sym_domain::basic_type::BasicType::*;
use crate::model::sym_domain::basic_type::TYPE_ID_EMPTY;
use crate::model::sym_domain::simple_type::SimpleType;
use crate::model::sym_domain::Domain::*;
use crate::model::sym_domain::{Domain, DomainSubstitution, TypeId};
use map_macro::hash_set;
use ompas_language::sym_table::{
    TYPE_ABSTRACT_TASK, TYPE_COMMAND, TYPE_METHOD, TYPE_OBJECT, TYPE_OBJECT_TYPE, TYPE_PREDICATE,
    TYPE_RESOURCE_HANDLE, TYPE_STATE_FUNCTION, TYPE_TASK, TYPE_TIMEPOINT,
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Write};
use std::ops::Deref;

#[derive(Clone, Debug)]
pub struct TypeLattice {
    pub(crate) types: Vec<SimpleType>,
    childs: Vec<Vec<TypeId>>,
    parents: Vec<Vec<TypeId>>,
    decomposition: Vec<Vec<TypeId>>,
    pub(crate) aliases: HashMap<TypeId, TypeId>,
    ids: HashMap<String, TypeId>,
}

impl Default for TypeLattice {
    fn default() -> Self {
        let mut ids: HashMap<String, TypeId> = Default::default();
        ids.insert(SimpleType::Basic(Empty).to_string().to_ascii_lowercase(), 0);
        ids.insert(SimpleType::Basic(Any).to_string().to_ascii_lowercase(), 0);
        let mut dc = Self {
            types: vec![SimpleType::Basic(Empty), SimpleType::Basic(Any)],
            childs: vec![vec![], vec![]],
            parents: vec![vec![], vec![]],
            decomposition: vec![vec![], vec![]],
            aliases: Default::default(),
            ids,
        };

        dc.add_type(Boolean, vec![]);
        dc.add_type(List, vec![]);
        dc.add_type(Map, vec![]);
        dc.add_type(Err, vec![]);
        dc.add_type(Handle, vec![]);
        dc.add_type(Number, vec![]);
        dc.add_type(Nil, vec![]);
        dc.add_type(Float, vec![Number as usize]);
        dc.add_type(Int, vec![Number as usize, Float as usize]);
        dc.add_decomposition(Number as usize, vec![Int as usize, Float as usize]);
        dc.add_type(Symbol, vec![]);
        dc.add_type(EmptyList, vec![List as usize, Nil as usize]);
        dc.add_type(True, vec![Boolean as usize]);
        dc.add_type(False, vec![Boolean as usize, Nil as usize]);
        dc.add_type(Proc, vec![]);
        dc.add_type(Primitive, vec![Proc as usize]);
        dc.add_type(Fn, vec![Proc as usize]);
        dc.add_type(Lambda, vec![Proc as usize]);
        dc.add_type(Vector, vec![List as usize]);
        dc.add_type(Tuple, vec![List as usize]);
        dc.add_decomposition(
            Proc as usize,
            vec![Primitive as usize, Fn as usize, Lambda as usize],
        );
        dc.add_decomposition(Nil as usize, vec![False as usize, EmptyList as usize]);
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
        //dc.add_alias(TYPE_BOOLEAN, TYPE_BOOL);

        dc
    }
}

impl TypeLattice {
    pub fn new() -> Self {
        let mut lt = Self::default();
        //Type linked to the resource representation
        lt.add_type(TYPE_RESOURCE_HANDLE, vec![]);

        //Type linked to the planning objects
        //lt.add_alias(Number, TYPE_TIMEPOINT);
        lt.add_type(TYPE_TIMEPOINT, vec![Float as usize, Int as usize]);
        //lt.add_type(TYPE_PRESENCE, vec![True as usize, False as usize]);
        //lt.add_alias(Boolean, TYPE_PRESENCE);

        lt.add_type(TYPE_ABSTRACT_TASK, vec![Symbol as usize]);
        lt.add_type(
            TYPE_COMMAND,
            vec![*lt.get_type_id(TYPE_ABSTRACT_TASK).unwrap()],
        );
        lt.add_type(
            TYPE_TASK,
            vec![*lt.get_type_id(TYPE_ABSTRACT_TASK).unwrap()],
        );
        lt.add_type(TYPE_METHOD, vec![Symbol as usize]);
        lt.add_type(TYPE_OBJECT, vec![Symbol as usize]);
        lt.add_type(TYPE_OBJECT_TYPE, vec![Symbol as usize]);
        lt.add_type(TYPE_STATE_FUNCTION, vec![Symbol as usize]);
        lt.add_type(
            TYPE_PREDICATE,
            vec![*lt.get_type_id(TYPE_STATE_FUNCTION).unwrap()],
        );
        lt
    }

    pub fn add_alias(&mut self, t: impl Display, alias: impl Display) -> TypeId {
        let alias: SimpleType = alias.to_string().into();
        let id = self.types.len();
        self.types.push(alias.clone());
        self.ids.insert(alias.to_string().to_ascii_lowercase(), id);
        self.childs.push(vec![]);
        self.parents.push(vec![]);
        self.decomposition.push(vec![]);
        let p_id = *self
            .ids
            .get(t.to_string().to_ascii_lowercase().as_str())
            .unwrap();
        self.aliases.insert(id, p_id);
        id
    }

    pub fn format_type(&self, id: &TypeId) -> String {
        self.types[*id].to_string()
    }

    pub fn get_type_id(&self, r#type: impl Display) -> Option<&TypeId> {
        self.ids.get(&r#type.to_string().to_ascii_lowercase())
    }

    pub fn get_parent(&self, id: &TypeId) -> Vec<TypeId> {
        self.parents[*id].clone()
    }

    pub fn add_type(&mut self, r#type: impl Into<SimpleType>, parents: Vec<TypeId>) -> TypeId {
        let r#type = r#type.into();
        let id = self.types.len();
        self.types.push(r#type.clone());
        self.ids.insert(r#type.to_string().to_ascii_lowercase(), id);
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

    pub fn add_parent(&mut self, id_type: TypeId, id_parent: TypeId) {
        let parents = &mut self.parents[id_type];
        if !parents.is_empty() && parents[0] == Any as usize {
            parents.remove(0);
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

    fn _substitute_aliases(&self, d: &Domain) -> (Domain, HashSet<DomainSubstitution>) {
        let mut substitutions: HashSet<_> = HashSet::default();
        let mut add_subs = |sub: HashSet<DomainSubstitution>| {
            for s in sub {
                substitutions.insert(s);
            }
        };

        let d = match d {
            Simple(s) => match self.aliases.get(s) {
                None => Simple(*s),
                Some(id) => {
                    add_subs(hash_set!(DomainSubstitution {
                        alias: *s,
                        base: *id,
                    }));
                    Simple(*id)
                }
            },
            Composed(t, c) => {
                let t = match self.aliases.get(t) {
                    None => *t,
                    Some(id) => {
                        add_subs(hash_set! {DomainSubstitution {
                            alias: *t,
                            base: *id,
                        }});
                        *id
                    }
                };

                Composed(
                    t,
                    c.iter()
                        .map(|c| {
                            let (t, sub) = self._substitute_aliases(c);
                            add_subs(sub);
                            t
                        })
                        .collect(),
                )
            }
            Union(u) => Union(
                u.iter()
                    .map(|d| {
                        let (d, subs) = self._substitute_aliases(d);
                        add_subs(subs);
                        d
                    })
                    .collect(),
            ),
            Substract(t, s) => {
                let (t, sub) = self._substitute_aliases(t);
                add_subs(sub);
                let (s, sub) = self._substitute_aliases(s);
                add_subs(sub);
                Substract(Box::new(t), Box::new(s))
            }
            Cst(t, c) => {
                let (t, sub) = self._substitute_aliases(t);
                add_subs(sub);
                Cst(Box::new(t), c.clone())
            }
            Application(a, args, r) => {
                let (a, sub) = self._substitute_aliases(a);
                add_subs(sub);
                let (r, sub) = self._substitute_aliases(r);
                add_subs(sub);
                Application(
                    Box::new(a),
                    args.iter()
                        .map(|d| {
                            let (d, sub) = self._substitute_aliases(d);
                            add_subs(sub);
                            d
                        })
                        .collect(),
                    Box::new(r),
                )
            }
            _ => d.clone(),
        };
        (d, substitutions)
    }

    pub fn contained_in(&self, d1: &Domain, d2: &Domain) -> bool {
        //let (d1, _) = &self.substitute_aliases(d1);
        //let (d2, _) = &self.substitute_aliases(d2);
        self.__contained_in(d1, d2)
    }

    fn __contained_in(&self, d1: &Domain, d2: &Domain) -> bool {
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
                if self.__contained_in(d1, d2) {
                    c1 == c2
                } else {
                    false
                }
            }
            (Composed(top1, comp1), Composed(top2, comp2)) => {
                if top1 == top2 && comp1.len() == comp2.len() {
                    for (d1, d2) in comp1.iter().zip(comp2) {
                        if !self.__contained_in(d1, d2) {
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
                    if !self.__contained_in(t1, d2) {
                        return false;
                    }
                }
                true
            }
            (Union(_), _) => false,
            (t, Union(u2)) => {
                for t2 in u2 {
                    if self.__contained_in(t, t2) {
                        return true;
                    }
                }
                false
            }
            (Substract(t1, _), t2) => self.__contained_in(t1.deref(), t2),
            (_, Substract(_, _)) => todo!(),

            (Cst(d1, _), t2) => self.__contained_in(d1, t2),
            (_, Cst(_, _)) => false,
            (Application(t1, params_1, r_1), Application(t2, params_2, r_2)) => {
                if self.__contained_in(t1, t2) {
                    if params_1.len() == params_2.len() {
                        for (p_1, p_2) in params_1.iter().zip(params_2) {
                            if !self.__contained_in(p_1, p_2) {
                                return false;
                            }
                        }
                        self.__contained_in(r_1, r_2)
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn meet(&self, t1: &Domain, t2: &Domain) -> Domain {
        //let (d1, subs) = self.substitute_aliases(t1);
        //let (d2, subs2) = self.substitute_aliases(t2);
        // let r = self.__meet(d1, d2);
        // r.substitute(subs.union(&subs2).cloned().collect())
        self.__meet(t1, t2)
    }

    fn __meet(&self, t1: &Domain, t2: &Domain) -> Domain {
        match (t1, t2) {
            (Simple(t1), Simple(t2)) => {
                if t1 == t2 {
                    Simple(*t1)
                } else {
                    let mut childs_t1 = self.get_all_childs(t1);
                    childs_t1.sort();
                    //println!("childs of {t1}: {:?}", childs_t1);
                    let mut childs_t2 = self.get_all_childs(t2);
                    childs_t2.sort();
                    //println!("childs of {t2}: {:?}", childs_t2);

                    for child_1 in &childs_t1 {
                        for child_2 in &childs_t2 {
                            if child_1 == child_2 {
                                return Simple(*child_1);
                            }
                        }
                    }
                    Simple(Empty as usize)
                }
            }
            (Simple(_), Composed(top2, _)) => {
                let nt2 = Simple(*top2);
                let meet = self.__meet(t1, &nt2);
                if meet.is_empty() {
                    meet
                } else if nt2 == meet {
                    t2.clone()
                } else {
                    meet
                }
            }
            (Composed(top1, _), Simple(_)) => {
                let nt1 = Simple(*top1);
                let meet = self.__meet(&nt1, t2);
                if meet.is_empty() {
                    meet
                } else if nt1 == meet {
                    t1.clone()
                } else {
                    meet
                }
            }
            (Composed(top1, comp1), Composed(top2, comp2)) => {
                if top1 == &TYPE_ID_EMPTY || top2 == &TYPE_ID_EMPTY {
                    return Domain::empty();
                }
                if top1 == top2 && comp1.len() == comp2.len() {
                    let mut comp = vec![];
                    for (t1, t2) in comp1.iter().zip(comp2) {
                        let meet = self.__meet(t1, t2);
                        if meet == Simple(Empty as usize) {
                            return Domain::empty();
                        }
                        comp.push(meet)
                    }
                    Composed(*top1, comp)
                } else {
                    self.__meet(&Simple(*top1), &Simple(*top2))
                }
            }
            (Union(ua), Union(_)) => {
                //println!("Meet {ta} and {tb}.");
                let mut meet = Domain::empty();
                for ta in ua {
                    //println!("Meet {ta} and {tb}.");
                    meet = self.__union(&self.__meet(ta, t2), &meet);
                }
                meet

                //return self.__union(&Union(meets.drain().collect()), &Domain::empty());
            }
            (Union(ua), t) => {
                let mut meet = Domain::empty();
                for tu in ua {
                    meet = self.__union(&self.__meet(tu, t), &meet);
                }
                meet
            }
            (t, Union(ub)) => {
                let mut meet = Domain::empty();
                for tu in ub {
                    meet = self.__union(&self.__meet(t, tu), &meet);
                }
                meet
            }
            (Substract(t1, t2), t3) => self.__substract(&self.__meet(t1, t3), &self.__meet(t2, t3)),
            (t1, Substract(t2, t3)) => self.__substract(&self.__meet(t1, t2), &self.__meet(t1, t3)),
            (Cst(d1, c1), Cst(d2, c2)) => {
                let meet = self.__meet(d1, d2);
                if meet != Simple(Empty as usize) && c1 == c2 {
                    return Cst(Box::new(meet), c1.clone());
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
                    Cst(Box::new(meet), c2.clone())
                } else {
                    Empty.into()
                }
            }
            (Application(_, _, _), Application(_, _, _)) => {
                //println!("meet({}, {})", t1.format(&self), t2.format(&self));
                todo!()
            }
            (Application(t_app, params, r), Simple(t2))
            | (Simple(t2), Application(t_app, params, r)) => {
                let t = self.__meet(t_app, &Simple(*t2));
                if t.is_empty() {
                    t
                } else {
                    Application(Box::new(t), params.clone(), r.clone())
                }
            }
            (_, _) => {
                //println!("meet({}, {})", t1.format(&self), t2.format(&self));
                todo!()
            }
        }
    }

    pub fn union(&self, t1: &Domain, t2: &Domain) -> Domain {
        // let (t1, subs) = self.substitute_aliases(t1);
        // let (t2, subs2) = self.substitute_aliases(t2);
        // self.__union(&t1, &t2)
        //     .substitute(subs.union(&subs2).cloned().collect())
        self.__union(t1, t2)
    }

    fn __union(&self, t1: &Domain, t2: &Domain) -> Domain {
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
                        Composed(*top1, comp)
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
                (Substract(t1, t2), t3) => self.__substract(
                    &self.__union(t1.deref(), t3),
                    &self.__substract(t2.deref(), &self.__meet(t2.deref(), t3)),
                ),
                (t1, Substract(t2, t3)) => self.__substract(
                    &self.__union(t1, t2.deref()),
                    &self.__substract(t3.deref(), &self.__meet(t3.deref(), t1)),
                ),

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
                _ => todo!(),
            }
        }
    }

    pub fn get_aliases(&self, t: &Domain) -> HashSet<(TypeId, TypeId)> {
        let mut set: HashSet<(_, _)> = Default::default();

        match t {
            Simple(t) => {
                if let Some(id) = self.aliases.get(t) {
                    set.insert((*t, *id));
                }
            }
            Composed(c, d) => {
                set = set.union(&self.get_aliases(&Simple(*c))).cloned().collect();
                for t in d {
                    set = set.union(&self.get_aliases(t)).cloned().collect();
                }
            }
            Union(u) => {
                for t in u {
                    set = set.union(&self.get_aliases(t)).cloned().collect();
                }
            }
            Substract(s, c) => {
                set = set.union(&self.get_aliases(s)).cloned().collect();
                set = set.union(&self.get_aliases(c)).cloned().collect();
            }
            Cst(t, _) => {
                set = self.get_aliases(t);
            }
            IntRange(_, _) => {}
            Application(f, args, r) => {
                set = set.union(&self.get_aliases(f)).cloned().collect();
                set = set.union(&self.get_aliases(r)).cloned().collect();
                for t in args {
                    set = set.union(&self.get_aliases(t)).cloned().collect();
                }
            }
        };
        set
    }

    pub fn substract(&self, t1: &Domain, t2: &Domain) -> Domain {
        //let (t2, subs) = self.substitute_aliases(t2);
        //let (t1, subs2) = self.substitute_aliases(t1);
        //self.__substract(&t1, &t2)
        //    .substitute(subs.union(&subs2).cloned().collect())
        self.__substract(t1, t2)
    }

    fn __substract(&self, t1: &Domain, t2: &Domain) -> Domain {
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
                (Substract(t1, t2), t3) => Substract(t1.clone(), Box::new(self.__union(t2, t3))),
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

    pub fn simplify_union(&self, set: HashSet<Domain>) -> Domain {
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
                    tested.insert(*t);
                    let parents: &Vec<TypeId> = &self.parents[*t];
                    for parent in parents {
                        let d = self.get_decomposition(parent);
                        if !d.is_empty() {
                            decompositions.push((*parent, d))
                        }
                    }
                }
            }
            if !decompositions.is_empty() {
                'main: for (p, decomposition) in decompositions.drain(..) {
                    for t in decomposition {
                        if !simples.contains(t) {
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

    pub fn export_dot(&self) -> String {
        let mut dot: String = "digraph {\n".to_string();

        writeln!(dot, "NONE [label = \"Empty\"]",).unwrap();

        for (id, domain) in self.types[1..].iter().enumerate() {
            let id = id + 1;
            if !self.aliases.contains_key(&id) {
                let aliases: Vec<SimpleType> = self
                    .aliases
                    .iter()
                    .filter_map(|(k, v)| {
                        if *v == id {
                            Some(self.types[*k].clone())
                        } else {
                            None
                        }
                    })
                    .collect();

                let aliases_str = if aliases.is_empty() {
                    "".to_string()
                } else {
                    let mut str = "\n(".to_string();
                    for (i, alias) in aliases.iter().enumerate() {
                        if i != 0 {
                            str.push(',');
                        }
                        str.push_str(alias.to_string().as_str())
                    }
                    str.push(')');
                    str
                };

                writeln!(
                    dot,
                    "{VERTICE_PREFIX}{} [label = \"{}{}\"]",
                    id, domain, aliases_str
                )
                .unwrap();

                let childs = &self.childs[id];
                if childs.is_empty() {
                    writeln!(dot, "{VERTICE_PREFIX}{id} -> NONE",).unwrap();
                } else {
                    for child in childs {
                        writeln!(dot, "{VERTICE_PREFIX}{id} -> {VERTICE_PREFIX}{child}",).unwrap();
                    }
                }
            }
        }

        dot.push('}');
        dot
    }
}

const VERTICE_PREFIX: &str = "D";
