use crate::structs::Sym::Unique;
use ompas_acting::rae::context::rae_env::DomainEnv;
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::language::scheme_primitives::*;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LNumber, LValue};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Hash, Eq, PartialEq, Clone)]
pub enum Sym {
    Unique(String),
    Several(String, usize),
}

impl Sym {
    pub fn get_string(&self) -> &String {
        match self {
            Sym::Unique(s) => s,
            Sym::Several(s, _) => s,
        }
    }
}

impl From<String> for Sym {
    fn from(s: String) -> Self {
        Self::Unique(s)
    }
}

impl From<&str> for Sym {
    fn from(s: &str) -> Self {
        Self::Unique(s.to_string())
    }
}

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unique(s) => write!(f, "{}", s),
            Self::Several(s, i) => write!(f, "{}({})", s, i),
        }
    }
}

type SymId = usize;

pub trait Absorb {
    fn absorb(&mut self, other: Self);
}

pub trait FormatWithSymTable {
    fn format_with_sym_table(&self, st: &SymTable) -> String;
}

#[derive(Clone, Copy, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub enum SymType {
    Timepoint,
    Result,
    Object,
    Action,
    StateFunction,
    Method,
    Task,
    Function,
}

impl Display for SymType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SymType::Timepoint => write!(f, "timepoint"),
            SymType::Result => write!(f, "return"),
            SymType::Object => write!(f, "object"),
            SymType::Action => write!(f, "action"),
            SymType::StateFunction => write!(f, "state-function"),
            SymType::Method => write!(f, "method"),
            SymType::Task => write!(f, "task"),
            SymType::Function => write!(f, "function"),
        }
    }
}

#[derive(Clone)]
pub struct SymTable {
    symbols: Vec<Sym>,
    ids: HashMap<Sym, SymId>,
    symbol_types: HashMap<SymId, SymType>,
    types_number: HashMap<SymType, usize>,
    multiple_def: HashMap<String, Vec<SymId>>,
    pointer_to_ver: Vec<HashMap<String, usize>>,
}

impl Default for SymTable {
    fn default() -> Self {
        let mut types_number = HashMap::new();
        types_number.insert(SymType::Result, 0);
        types_number.insert(SymType::Timepoint, 0);
        types_number.insert(SymType::Object, 0);
        types_number.insert(SymType::Action, 0);
        types_number.insert(SymType::StateFunction, 0);
        types_number.insert(SymType::Method, 0);
        types_number.insert(SymType::Task, 0);
        types_number.insert(SymType::Function, 0);

        let mut st = Self {
            symbols: vec![],
            ids: Default::default(),
            symbol_types: Default::default(),
            types_number,
            multiple_def: Default::default(),
            pointer_to_ver: vec![Default::default()],
        };

        //Symbols of lisp functions that are useful
        //Not exhaustive
        st.add_list_of_symbols_of_same_type(
            get_scheme_primitives()
                .iter()
                .map(|s| s.to_string())
                .collect(),
            &SymType::Function,
        )
        .expect("error while adding symbols of scheme primitives");

        st
    }
}

impl SymTable {
    pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<String>,
        sym_type: &SymType,
    ) -> Result<(), LError> {
        let types_number = *self.types_number.get(sym_type).unwrap();

        for element in &list {
            if self.it_exists(element) {
                return Err(SpecialError(
                    "add_list_of_symbols_of_same_type",
                    format!("{} already exists", element),
                ));
            }

            let id = self.symbols.len();
            self.symbols.push(element.clone().into());
            self.ids.insert(element.clone().into(), id);
            self.symbol_types.insert(id, *sym_type);
        }

        self.types_number
            .insert(*sym_type, types_number + list.len());

        Ok(())
    }
}

impl SymTable {
    pub fn get(&self, id: &SymId) -> Option<&Sym> {
        self.symbols.get(*id)
    }

    pub fn id(&self, sym: &str) -> Option<&SymId> {
        //Look before in the multiple_def table, and then looking in self.ids
        if self.multiple_def.contains_key(sym) {
            let ver = self.pointer_to_ver.last().unwrap().get(sym).unwrap();
            let value = self.multiple_def.get(sym).unwrap().get(*ver);
            value
        } else {
            self.ids.get(&sym.into())
        }
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn declare_new_result(&mut self) -> SymId {
        let n = self.types_number.get_mut(&SymType::Result).unwrap();
        let sym = format!("r_{}", n);
        *n += 1;
        let id = self.symbols.len();
        self.symbols.push(sym.clone().into());
        self.ids.insert(sym.into(), id);
        self.symbol_types.insert(id, SymType::Result);
        id
    }

    pub fn unique_to_several(&mut self, sym: &str) {
        if !self.multiple_def.contains_key(sym) {
            //change value in vec of symbol
            let id = self.ids.remove(&Unique(sym.to_string())).unwrap();
            self.symbols[id] = Sym::Several(sym.to_string(), 0);
            //Update key in hashmap
            self.ids.insert(self.symbols[id].clone(), id);
            //Create new entry in multiple_def
            self.multiple_def.insert(sym.to_string(), vec![id]);
            self.pointer_to_ver
                .last_mut()
                .unwrap()
                .insert(sym.to_string(), 0);
        }
    }

    pub fn declare_new_interval(&mut self) -> Interval {
        let n = self.types_number.get_mut(&SymType::Timepoint).unwrap();
        let start: Sym = format!("t_{}", n).into();
        let end: Sym = format!("t_{}", *n + 1).into();
        *n += 2;
        let id_1 = self.symbols.len();
        let id_2 = id_1 + 1;
        self.symbols.push(start.clone());
        self.symbols.push(end.clone());
        self.ids.insert(start, id_1);
        self.symbol_types.insert(id_1, SymType::Timepoint);
        self.ids.insert(end, id_2);
        self.symbol_types.insert(id_2, SymType::Timepoint);
        Interval {
            start: id_1,
            end: id_2,
        }
    }

    pub fn new_scope(&mut self) {
        self.pointer_to_ver
            .push(self.pointer_to_ver.last().unwrap().clone())
    }

    pub fn revert_scope(&mut self) {
        self.pointer_to_ver.remove(self.pointer_to_ver.len() - 1);
    }

    pub fn declare_new_timepoint(&mut self) -> SymId {
        let n = self.types_number.get_mut(&SymType::Timepoint).unwrap();
        let sym: Sym = format!("t_{}", n).into();
        *n += 1;
        let id = self.symbols.len();
        self.symbols.push(sym.clone());
        self.ids.insert(sym, id);
        self.symbol_types.insert(id, SymType::Timepoint);
        id
    }

    pub fn it_exists(&self, sym: &str) -> bool {
        self.ids.keys().any(|k| k.get_string() == sym)
    }

    pub fn declare_new_object(
        &mut self,
        obj: Option<String>,
        if_it_exists_create_new: bool,
    ) -> SymId {
        let n = self.types_number.get_mut(&SymType::Object).unwrap();
        let temp_n = *n;
        *n += 1;
        let id = self.symbols.len();
        let sym: Sym = match obj {
            None => format!("o_{}", temp_n).into(),
            Some(s) => {
                if self.it_exists(&s) {
                    if if_it_exists_create_new {
                        self.unique_to_several(&s);
                        let vec_similar = self.multiple_def.get_mut(&s).unwrap();
                        let n = vec_similar.len();
                        vec_similar.push(id);
                        *self.pointer_to_ver.last_mut().unwrap().get_mut(&s).unwrap() =
                            vec_similar.len() - 1;
                        Sym::Several(s, n)
                    } else {
                        return *match self.pointer_to_ver.last().unwrap().get(&s) {
                            None => self.ids.get(&s.into()).unwrap(),
                            Some(i) => self.multiple_def.get(&s).unwrap().get(*i).unwrap(),
                        };
                    }
                } else {
                    s.into()
                }
            }
        };

        self.symbols.push(sym.clone());
        self.ids.insert(sym, id);
        self.symbol_types.insert(id, SymType::Object);
        id
    }
}

impl FormatWithSymTable for PartialChronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let get_sym = |id: &SymId| {
            st.get(id)
                .expect("error in the definition of the symbol_table")
        };

        let mut s = String::new();
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| get_sym(id).to_string())
            .collect::<Vec<String>>();
        variables.sort();

        for (i, sym) in variables.iter().enumerate() {
            if i != 0 {
                s.push(',');
            }
            s.push_str(sym);
        }
        s.push_str("}\n");

        s.push_str("-constraint(s): {\n");
        for c in &self.constraints {
            s.push('\t');
            s.push_str(c.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {\n");
        for e in &self.conditions {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {\n");
        for e in &self.effects {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {\n");
        for e in &self.subtasks {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }
}

impl FormatWithSymTable for Chronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let get_sym = |id: &SymId| {
            st.get(id)
                .expect("error in the definition of the symbol_table")
        };

        let mut s = String::new();
        //name
        s.push_str("-name: ");
        for e in &self.name {
            s.push_str(get_sym(e).to_string().as_str());
            s.push(' ');
        }
        s.push('\n');
        //task
        s.push_str("-task: ");
        for e in &self.task {
            s.push_str(get_sym(e).to_string().as_str());
            s.push(' ');
        }
        s.push('\n');
        s.push_str(self.partial_chronicle.format_with_sym_table(st).as_str());
        s
    }
}

#[derive(Clone, Default)]
pub struct Chronicle {
    name: Vec<SymId>,
    task: Vec<SymId>,
    partial_chronicle: PartialChronicle,
}

impl Chronicle {
    pub fn absorb_expression_chronicle(&mut self, ec: ExpressionChronicle) {
        self.partial_chronicle.absorb(ec.partial_chronicle);
        //add result
        self.add_var(&ec.result);

        //add interval
        self.add_interval(&ec.interval);

        //add new subtask
    }
}

impl Chronicle {
    pub fn add_var(&mut self, sym_id: &SymId) {
        self.partial_chronicle.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.partial_chronicle.add_interval(interval);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.partial_chronicle.add_constraint(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.partial_chronicle.add_condition(cond)
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.partial_chronicle.add_effect(effect)
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.partial_chronicle.add_subtask(sub_task)
    }

    pub fn set_name(&mut self, name: Vec<SymId>) {
        self.name = name;
    }

    pub fn set_task(&mut self, task: Vec<SymId>) {
        self.task = task;
    }
}

#[derive(Clone, Default)]
pub struct PartialChronicle {
    variables: HashSet<SymId>,
    constraints: Vec<Constraint>,
    conditions: Vec<Condition>,
    effects: Vec<Effect>,
    subtasks: Vec<Expression>,
}

impl Absorb for PartialChronicle {
    fn absorb(&mut self, mut other: Self) {
        self.variables = self.variables.union(&other.variables).copied().collect();
        self.constraints.append(&mut other.constraints);
        self.conditions.append(&mut other.conditions);
        self.effects.append(&mut other.effects);
        self.subtasks.append(&mut other.subtasks);
    }
}

impl PartialChronicle {
    pub fn add_var(&mut self, sym_id: &SymId) {
        self.variables.insert(*sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.variables.insert(interval.start);
        self.variables.insert(interval.end);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.conditions.push(cond);
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.effects.push(effect);
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.subtasks.push(sub_task);
    }
}

pub struct ExpressionChronicle {
    interval: Interval,
    result: SymId,
    partial_chronicle: PartialChronicle,
    value: Lit,
    debug: LValue,
}

impl ExpressionChronicle {
    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_result(&self) -> &SymId {
        &self.result
    }
}

//Creates a new expression chronicle, declaring an interval and a result variable in the symbol table.
//The LValue is used for debug
impl ExpressionChronicle {
    pub fn new(lv: LValue, st: &mut SymTable) -> Self {
        let interval = st.declare_new_interval();
        let result = st.declare_new_result();
        Self {
            interval,
            result,
            partial_chronicle: Default::default(),
            value: Lit::Exp(vec![]),
            debug: lv,
        }
    }

    pub fn set_lit(&mut self, lit: Lit) {
        self.value = lit;
    }

    pub fn get_lit(&self) -> Lit {
        self.value.clone()
    }
}

impl ExpressionChronicle {
    pub fn add_var(&mut self, sym_id: &SymId) {
        self.partial_chronicle.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.partial_chronicle.add_interval(interval);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.partial_chronicle.add_constraint(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.partial_chronicle.add_condition(cond)
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.partial_chronicle.add_effect(effect)
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.partial_chronicle.add_subtask(sub_task)
    }
}

impl Absorb for ExpressionChronicle {
    fn absorb(&mut self, other: Self) {
        self.partial_chronicle.absorb(other.partial_chronicle);
        self.add_interval(&other.interval);
        self.add_var(&other.result);
    }
}

impl FormatWithSymTable for ExpressionChronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let get_sym = |id: &SymId| {
            st.get(id)
                .expect("error in the definition of the symbol_table")
        };
        let mut s = String::new();

        s.push_str(
            format!(
                "{} {} <- {}\n",
                self.interval.format_with_sym_table(st),
                get_sym(&self.result),
                self.debug
            )
            .as_str(),
        );
        s.push_str(
            format!(
                "subchronicle: \n{}",
                self.partial_chronicle.format_with_sym_table(st)
            )
            .as_str(),
        );

        s
    }
}
#[derive(Clone)]
pub struct Condition {
    pub interval: Interval,
    pub constraint: Constraint,
}

impl FormatWithSymTable for Condition {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.constraint.format_with_sym_table(st)
        )
    }
}

#[derive(Clone)]
pub struct TransitionInterval {
    interval: Interval,
    persistence: SymId,
}

impl TransitionInterval {
    pub fn new(interval: Interval, persistence: SymId) -> Self {
        Self {
            interval,
            persistence,
        }
    }
}

impl FormatWithSymTable for TransitionInterval {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "[{},{},{}]",
            st.get(&self.interval.start).unwrap(),
            st.get(&self.interval.end).unwrap(),
            st.get(&self.persistence).unwrap()
        )
    }
}

#[derive(Clone)]
pub struct Transition {
    variable: Lit,
    value: Lit,
}

impl Transition {
    pub fn new(var: Lit, val: Lit) -> Self {
        Self {
            variable: var,
            value: val,
        }
    }
}

impl FormatWithSymTable for Transition {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} <- {}",
            self.variable.format_with_sym_table(st),
            self.value.format_with_sym_table(st)
        )
    }
}

#[derive(Clone)]
pub struct Effect {
    pub interval: Interval,
    pub transition: Transition,
}

impl FormatWithSymTable for Effect {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.transition.format_with_sym_table(st)
        )
    }
}

#[derive(Clone)]
pub enum Lit {
    Atom(SymId),
    Number(LNumber),
    Boolean(bool),
    LValue(LValue),
    Constraint(Box<Constraint>),
    Exp(Vec<Lit>),
}

impl From<&LNumber> for Lit {
    fn from(n: &LNumber) -> Self {
        Self::Number(n.clone())
    }
}

impl From<LNumber> for Lit {
    fn from(n: LNumber) -> Self {
        (&n).into()
    }
}

impl From<bool> for Lit {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}

impl From<&str> for Lit {
    fn from(s: &str) -> Self {
        Self::LValue(s.into())
    }
}

impl From<&SymId> for Lit {
    fn from(s: &SymId) -> Self {
        Self::Atom(*s)
    }
}

impl From<SymId> for Lit {
    fn from(s: SymId) -> Self {
        (&s).into()
    }
}

impl From<&LValue> for Lit {
    fn from(lv: &LValue) -> Self {
        Self::LValue(lv.clone())
    }
}

impl From<LValue> for Lit {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl From<&Constraint> for Lit {
    fn from(c: &Constraint) -> Self {
        Self::Constraint(Box::new(c.clone()))
    }
}

impl From<Constraint> for Lit {
    fn from(c: Constraint) -> Self {
        (&c).into()
    }
}

impl<T: Clone + Into<Lit>> From<&Vec<T>> for Lit {
    fn from(v: &Vec<T>) -> Self {
        Lit::Exp(v.iter().map(|e| e.clone().into()).collect())
    }
}

impl<T: Clone + Into<Lit>> From<Vec<T>> for Lit {
    fn from(v: Vec<T>) -> Self {
        (&v).into()
    }
}

pub fn lvalue_to_lit(lv: &LValue, st: &mut SymTable) -> Result<Lit, LError> {
    match lv {
        LValue::List(list) => {
            let mut vec = vec![];
            for e in list {
                vec.push(lvalue_to_lit(e, st)?);
            }
            Ok(vec.into())
        }
        LValue::Map(_) => Err(SpecialError(
            "LValue to lit",
            "Map transformation to lit is not supported yet.".to_string(),
        )),
        LValue::Number(n) => Ok(n.into()),
        LValue::True => Ok(true.into()),
        LValue::Nil => Ok(false.into()),
        lv => Ok(st.declare_new_object(Some(lv.to_string()), false).into()),
    }
}

impl FormatWithSymTable for Lit {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Lit::Atom(a) => st.symbols.get(*a).unwrap().to_string(),
            Lit::Constraint(c) => c.format_with_sym_table(st),
            Lit::Exp(vec) => {
                let mut str = "(".to_string();
                for (i, e) in vec.iter().enumerate() {
                    if i != 0 {
                        str.push(' ');
                    }
                    str.push_str(e.format_with_sym_table(st).as_str())
                }
                str.push(')');
                str
            }
            Lit::LValue(lv) => lv.to_string(),
            Lit::Number(n) => n.to_string(),
            Lit::Boolean(b) => match b {
                true => "true".to_string(),
                false => "nil".to_string(),
            },
        }
    }
}

#[derive(Clone)]
pub enum Constraint {
    Eq(Lit, Lit),
    Neg(Lit),
    LT(Lit, Lit),
    And(Lit, Lit),
    Or(Lit, Lit),
}

impl FormatWithSymTable for Constraint {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        match self {
            Constraint::Eq(l1, l2) => format!(
                "({} = {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::Neg(l1) => format!("(! {})", l1.format_with_sym_table(st)),
            Constraint::LT(l1, l2) => format!(
                "({} < {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::And(l1, l2) => format!(
                "({} && {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
            Constraint::Or(l1, l2) => format!(
                "({} || {})",
                l1.format_with_sym_table(st),
                l2.format_with_sym_table(st)
            ),
        }
    }
}

#[derive(Clone)]
pub struct Expression {
    pub interval: Interval,
    pub lit: Lit,
}

impl FormatWithSymTable for Expression {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.lit.format_with_sym_table(st)
        )
    }
}

#[derive(Copy, Clone)]
pub struct Interval {
    start: SymId,
    end: SymId,
}

impl Interval {
    pub fn new(start: &SymId, end: &SymId) -> Self {
        Self {
            start: *start,
            end: *end,
        }
    }
}

impl Interval {
    pub fn start(&self) -> SymId {
        self.start
    }

    pub fn end(&self) -> SymId {
        self.end
    }
}

impl FormatWithSymTable for Interval {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "[{},{}]",
            st.symbols.get(self.start).unwrap(),
            st.symbols.get(self.end).unwrap()
        )
    }
}

pub struct Context {
    pub domain: DomainEnv,
    pub env: LEnv,
    pub ctxs: ContextCollection,
}

pub struct Problem {}

type Action = Chronicle;
type Method = Chronicle;

#[derive(Default)]
pub struct Domain {
    actions: Vec<Action>,
    tasks: Vec<Lit>,
    methods: Vec<Method>,
}

impl Domain {
    pub fn new(actions: Vec<Action>, tasks: Vec<Lit>, methods: Vec<Method>) -> Self {
        Self {
            actions,
            tasks,
            methods,
        }
    }
}

impl FormatWithSymTable for Domain {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let mut str = String::new();

        str.push_str("DOMAIN:\n");

        //actions
        str.push_str("ACTIONS: \n");
        for action in &self.actions {
            str.push_str(format!("{}\n", action.format_with_sym_table(st)).as_str());
        }

        //tasks
        str.push_str("TASKS: \n");
        for task in &self.tasks {
            str.push_str(format!("{}\n", task.format_with_sym_table(st)).as_str());
        }

        //methods
        str.push_str("METHODS: \n");
        for method in &self.methods {
            str.push_str(format!("{}\n", method.format_with_sym_table(st)).as_str());
        }

        str
    }
}
