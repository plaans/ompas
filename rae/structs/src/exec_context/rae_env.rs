use crate::exec_context::job::Job;
use crate::exec_context::rae_state::RAEState;
use crate::refinement::Agenda;
use im::HashMap;
use sompas_core::get_root_env;
use sompas_language::{LIST, OBJECT};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lerror::LRuntimeError;

use sompas_structs::llambda::LLambda;
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::typelvalue::KindLValue;
use sompas_structs::{lerror, wrong_n_args, wrong_type};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;
use tokio::sync::mpsc::Receiver;

pub const RAE_TASK_METHODS_MAP: &str = "rae-task-methods-map";
pub const RAE_TASK_LIST: &str = "rae-task-list";
pub const RAE_METHOD_LIST: &str = "rae-methods-list";
pub const RAE_ACTION_LIST: &str = "rae-actions-list";
pub const RAE_STATE_FUNCTION_LIST: &str = "rae-state-function-list";
pub const RAE_SYMBOL_TYPE: &str = "rae-symbol-type";
pub const RAE_METHOD_TYPES_MAP: &str = "rae-method-types-map";
pub const RAE_METHOD_SCORE_MAP: &str = "rae-method-score-map";
pub const RAE_METHOD_GENERATOR_MAP: &str = "rae-method-generator-map";
pub const RAE_METHOD_PRE_CONDITIONS_MAP: &str = "rae-method-pre-conditions-map";
pub const RAE_ACTION_MODEL_MAP: &str = "rae-action-model-map";
pub const ACTION_TYPE: &str = "action_type";
pub const TASK_TYPE: &str = "task_type";
pub const METHOD_TYPE: &str = "method_type";
pub const STATE_FUNCTION_TYPE: &str = "state_function_type";
pub const LAMBDA_TYPE: &str = "lambda_type";

pub const TUPLE_TYPE: &str = "tuple";
pub const TYPE_LIST: &str = "tlist";

#[derive(Debug, Clone)]
pub enum Type {
    Single(Arc<Sym>),
    List(Box<Type>),
    Tuple(Vec<Type>),
}

impl Type {
    pub fn try_as_single(&self) -> Option<&String> {
        if let Self::Single(s) = self {
            Some(s.as_ref())
        } else {
            None
        }
    }
}

impl From<Type> for LValue {
    fn from(t: Type) -> Self {
        match t {
            Type::Single(s) => LValue::Symbol(s),
            Type::List(l) => vec![LValue::from(TYPE_LIST), l.deref().into()].into(),
            Type::Tuple(tuple) => {
                let mut vec: Vec<LValue> = vec![TUPLE_TYPE.into()];
                for t in tuple {
                    vec.push(t.into())
                }
                vec.into()
            }
        }
    }
}

/*impl From<Type> for LValue {
    fn from(t: Type) -> Self {
        (&t).into()
    }
}*/

impl From<&str> for Type {
    fn from(str: &str) -> Self {
        Self::Single(Arc::new(str.to_string()))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Type::Single(s) => s.to_string(),
            Type::List(t) => format!("[{}]", t),
            Type::Tuple(t) => {
                let mut str = "(".to_string();
                for (i, e) in t.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    str.push_str(format!("{}", e).as_str())
                }
                str.push(')');
                str
            }
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone)]
pub struct Parameters {
    inner: Vec<(Arc<Sym>, Type)>,
}

impl Parameters {
    pub fn inner(&self) -> &Vec<(Arc<Sym>, Type)> {
        &self.inner
    }
}

const TYPE_TRY_FROM_LVALUE: &str = "Type::try_from<&LValue>";

impl TryFrom<&LValue> for Type {
    type Error = LRuntimeError;

    fn try_from(lv: &LValue) -> Result<Self, Self::Error> {
        let err = lerror!(
            TYPE_TRY_FROM_LVALUE,
            format!("{} or {} was expected", TUPLE_TYPE, LIST)
        );

        match lv {
            LValue::Symbol(s) => {
                let string = if s.as_str() == LIST {
                    TYPE_LIST.to_string()
                } else {
                    s.to_string()
                };
                Ok(Self::Single(Arc::new(string)))
            }
            LValue::List(list) => {
                assert!(list.len() >= 2);
                if let LValue::Symbol(s) = &list[0] {
                    match s.as_str() {
                        TUPLE_TYPE => Ok(Self::Tuple({
                            let mut vec: Vec<Type> = Vec::with_capacity(list.len() - 1);
                            for e in &list[1..] {
                                vec.push(e.try_into()?)
                            }
                            vec
                        })),
                        LIST => {
                            assert_eq!(list.len(), 2);
                            Ok(Self::List(Box::new((&list[1]).try_into()?)))
                        }
                        _ => Err(err),
                    }
                } else {
                    Err(err)
                }
            }
            _ => Err(LRuntimeError::not_in_list_of_expected_types(
                TYPE_TRY_FROM_LVALUE,
                lv,
                vec![KindLValue::Symbol, KindLValue::List],
            )),
        }
    }
}

const PARAMETERS_TRY_FROM_LVALUE: &str = "Parameters::TryFrom(LValue)";

impl TryFrom<&LValue> for Parameters {
    type Error = LRuntimeError;

    #[function_name::named]
    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        let mut vec: Vec<(Arc<Sym>, Type)> = vec![];

        if let LValue::List(list) = value {
            for e in list.iter() {
                match e {
                    LValue::List(description) => {
                        if description.len() == 2 {
                            vec.push((
                                Arc::<Sym>::try_from(&description[0])
                                    .map_err(|e| e.chain(function_name!()))?,
                                Type::try_from(&description[1])
                                    .map_err(|e| e.chain(function_name!()))?,
                            ));
                        } else {
                            return Err(wrong_n_args!(
                                PARAMETERS_TRY_FROM_LVALUE,
                                description.as_slice(),
                                2
                            ));
                        }
                    }
                    LValue::Symbol(s) => {
                        vec.push((s.clone(), OBJECT.into()));
                    }
                    _ => {
                        return Err(LRuntimeError::not_in_list_of_expected_types(
                            PARAMETERS_TRY_FROM_LVALUE,
                            &e,
                            vec![KindLValue::List, KindLValue::Symbol],
                        ))
                    }
                }
            }
        } else if let LValue::Nil = &value {
        } else {
            return Err(wrong_type!(
                PARAMETERS_TRY_FROM_LVALUE,
                value,
                KindLValue::List
            ));
        }

        Ok(Self { inner: vec })
    }
}

impl TryFrom<LValue> for Parameters {
    type Error = LRuntimeError;

    fn try_from(v: LValue) -> Result<Self, Self::Error> {
        (&v).try_into()
    }
}

impl Parameters {
    pub fn get_params(&self) -> Vec<Arc<Sym>> {
        self.inner.iter().map(|tuple| tuple.0.clone()).collect()
    }

    pub fn get_types(&self) -> Vec<Type> {
        self.inner.iter().map(|tuple| tuple.1.clone()).collect()
    }

    pub fn get_types_as_lvalue(&self) -> LValue {
        self.inner
            .iter()
            .map(|(_, t)| t.into())
            .collect::<Vec<LValue>>()
            .into()
    }
    pub fn get_params_as_lvalue(&self) -> LValue {
        self.inner
            .iter()
            .map(|(p, _)| p.into())
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_number(&self) -> usize {
        self.inner.len()
    }
}

impl Display for Parameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = '('.to_string();
        for (i, e) in self.inner.iter().enumerate() {
            if i != 0 {
                str.push(' ');
            }
            str.push_str(format!("({} {})", e.0, e.1).as_str());
        }

        str.push(')');

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    task_label: String,
    parameters: Parameters,
    lambda_pre_conditions: LValue,
    lambda_score: LValue,
    lambda_body: LValue,
}

//Getters
impl Method {
    pub fn get_task_label(&self) -> &String {
        &self.task_label
    }

    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_pre_conditions(&self) -> &LValue {
        &self.lambda_pre_conditions
    }

    pub fn get_body(&self) -> &LValue {
        &self.lambda_body
    }

    pub fn get_lambda(&self) -> LValue {
        let l1: LLambda = (&self.lambda_pre_conditions).try_into().expect("");
        let l2: LLambda = (&self.lambda_body).try_into().expect("");
        let body: LValue = vec![
            LCoreOperator::Do.into(),
            l1.get_body().clone(),
            l2.get_body().clone(),
        ]
        .into();
        let mut env = l1.get_env_symbols();
        env.set_outer(l2.get_env_symbols());
        LLambda::new(l1.get_params(), body, env).into()
    }
}

impl Method {
    pub fn new(
        task_label: String,
        parameters: Parameters,
        conds: LValue,
        score: LValue,
        body: LValue,
    ) -> Self {
        Self {
            task_label,
            parameters,
            lambda_pre_conditions: conds,
            lambda_score: score,
            lambda_body: body,
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "-task: {}\n\
            -parameters: {}\n\
            -pre-conditions: {}\n\
            -score: {}\n\
            -body: {}\n",
            self.task_label,
            self.parameters,
            self.lambda_pre_conditions.format("pre-conditions: ".len()),
            self.lambda_score.format("score: ".len()),
            self.lambda_body.format("body: ".len())
        )
    }
}

#[derive(Debug, Clone)]
pub struct Task {
    body: LValue,
    label: String,
    parameters: Parameters,
    methods: Vec<String>,
}

impl Task {
    pub fn new(label: String, body: LValue, parameters: Parameters) -> Self {
        Self {
            body,
            label,
            parameters,
            methods: vec![],
        }
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }

    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_methods(&self) -> &Vec<String> {
        &self.methods
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }
}

impl Display for Task {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str_methods: String = '('.into();
        for (index, e) in self.methods.iter().enumerate() {
            str_methods.push_str(e.as_str());
            if index < self.methods.len() - 1 {
                str_methods.push(',');
            }
        }
        str_methods.push(')');

        write!(
            f,
            "-parameters: {}\n\
            -body: {}\n\
            -methods: {}\n",
            self.parameters,
            self.body.format("body: ".len()),
            str_methods
        )
    }
}

#[derive(Debug, Clone)]
pub struct StateFunction {
    parameters: Parameters,
    body: LValue,
}

impl StateFunction {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }
}

impl StateFunction {
    pub fn new(parameters: Parameters, body: LValue) -> Self {
        Self { parameters, body }
    }
}

impl Display for StateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "parameters : {}\nbody: {}",
            self.parameters,
            self.body.format("exec: ".len()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Action {
    label: String,
    parameters: Parameters,
    exec: LValue,
    sim: LValue,
}

impl Action {
    pub fn get_parameters(&self) -> &Parameters {
        &self.parameters
    }

    pub fn get_exec(&self) -> &LValue {
        &self.exec
    }

    pub fn get_sim(&self) -> &LValue {
        &self.sim
    }

    pub fn get_label(&self) -> &String {
        &self.label
    }
}

impl Action {
    pub fn new(label: impl Display, parameters: Parameters, exec: LValue, sim: LValue) -> Self {
        Self {
            label: label.to_string(),
            parameters,
            exec,
            sim,
        }
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "label: {}, parameters : {}\n exec: {}\n sim: {} ",
            self.label,
            self.parameters,
            self.exec.format("exec: ".len()),
            self.sim.format("sim: ".len())
        )
    }
}

type TypeId = usize;

#[derive(Default, Debug, Clone)]
pub struct TypeHierarchy {
    inner: HashMap<TypeId, String>,
    reverse: HashMap<String, TypeId>,
    parent: HashMap<TypeId, Option<TypeId>>,
    child: HashMap<TypeId, Vec<TypeId>>,
}

impl TypeHierarchy {
    pub fn get_id(&self, t: impl Display) -> Option<&TypeId> {
        self.reverse.get(&t.to_string())
    }

    pub fn get_symbol(&self, id: &TypeId) -> Option<&String> {
        self.inner.get(id)
    }

    pub fn add_type(&mut self, t: String, parent: Option<String>) {
        if let Some(p) = &parent {
            let parent_id = match self.get_id(p) {
                None => {
                    let id = self.inner.len();
                    self.inner.insert(id, p.to_string());
                    self.reverse.insert(p.to_string(), id);
                    self.parent.insert(id, None);
                    self.child.insert(id, vec![]);
                    id
                }
                Some(id) => *id,
            };
            let child_id = self.inner.len();
            self.child.get_mut(&parent_id).unwrap().push(child_id);
            self.inner.insert(child_id, t.to_string());
            self.reverse.insert(t, child_id);
            self.parent.insert(child_id, Some(parent_id));
            self.child.insert(child_id, vec![]);
        } else {
            let id = self.inner.len();
            self.inner.insert(id, t.to_string());
            self.reverse.insert(t, id);
            self.child.insert(id, vec![]);
            self.parent.insert(id, None);
        }
    }

    pub fn get_direct_parent(&self, t: &str) -> Option<&String> {
        if let Some(id) = self.get_id(t) {
            match self.parent.get(id).unwrap() {
                None => None,
                Some(p) => self.get_symbol(p),
            }
        } else {
            None
        }
    }

    pub fn get_parents(&self, t: &str) -> Vec<String> {
        let mut parents = vec![];
        let mut parent = self.get_direct_parent(t);
        while let Some(p) = parent {
            parents.push(p.clone());
            parent = self.get_direct_parent(p)
        }
        parents
    }

    pub fn get_direct_childs(&self, t: &str) -> Option<Vec<String>> {
        if let Some(id) = self.get_id(t) {
            self.child.get(id).map(|p| {
                p.iter()
                    .map(|id| self.get_symbol(id).unwrap().clone())
                    .collect()
            })
        } else {
            None
        }
    }

    pub fn get_childs(&self, t: &str) -> Vec<String> {
        let mut all_childs = vec![];

        let childs = self.get_direct_childs(t);
        if let Some(mut childs) = childs {
            while let Some(child) = childs.pop() {
                all_childs.push(child.clone());
                let child_childs = self.get_direct_childs(&child);
                if let Some(mut new_childs) = child_childs {
                    childs.append(&mut new_childs)
                }
            }
        }
        all_childs
    }

    fn format_childs(&self, id: &TypeId, level: usize) -> String {
        let mut str = format!("{}- {}\n", "\t".repeat(level), self.get_symbol(id).unwrap());
        for c in self.child.get(id).unwrap() {
            str.push_str(self.format_childs(c, level + 1).as_str());
        }
        str
    }

    pub fn format_hierarchy(&self) -> String {
        let root_types: Vec<TypeId> = self
            .parent
            .iter()
            .filter(|(_, &v)| v == None)
            .map(|(k, _)| *k)
            .collect();
        let mut str = "Type Hierarchy\n".to_string();
        for r in &root_types {
            str.push_str(self.format_childs(r, 0).as_str());
            str.push('\n');
        }
        str
    }

    pub fn get_tuple_type_parent(&self) -> Vec<(&String, Option<&String>)> {
        let mut vec = vec![];
        for (t, p) in &self.parent {
            vec.push((
                self.get_symbol(t).unwrap(),
                p.map(|id| self.inner.get(&id).unwrap()),
            ));
        }
        vec
    }
}

#[derive(Default, Debug, Clone)]
pub struct DomainEnv {
    tasks: HashMap<String, Task>,
    methods: HashMap<String, Method>,
    state_functions: HashMap<String, StateFunction>,
    actions: HashMap<String, Action>,
    lambdas: HashMap<String, LValue>,
    types: TypeHierarchy,
    map_symbol_type: HashMap<String, String>,
}

impl DomainEnv {
    pub fn get_element_description(&self, label: &str) -> String {
        match self.map_symbol_type.get(label) {
            None => format!("Keyword {} is not defined in the domain.", label),
            Some(s) => match s.as_str() {
                TASK_TYPE => self.tasks.get(label).unwrap().to_string(),
                METHOD_TYPE => self.methods.get(label).unwrap().to_string(),
                STATE_FUNCTION_TYPE => self.state_functions.get(label).unwrap().to_string(),
                LAMBDA_TYPE => self.lambdas.get(label).unwrap().to_string(),
                ACTION_TYPE => self.actions.get(label).unwrap().to_string(),
                _ => panic!("There should no other type of symbol_type"),
            },
        }
    }
}

//Getter
impl DomainEnv {
    pub fn get_tasks(&self) -> &HashMap<String, Task> {
        &self.tasks
    }

    pub fn get_methods(&self) -> &HashMap<String, Method> {
        &self.methods
    }

    pub fn get_state_functions(&self) -> &HashMap<String, StateFunction> {
        &self.state_functions
    }

    pub fn get_actions(&self) -> &HashMap<String, Action> {
        &self.actions
    }

    pub fn get_lambdas(&self) -> &HashMap<String, LValue> {
        &self.lambdas
    }
}

//Adder
impl DomainEnv {
    pub fn add_task(&mut self, label: String, value: Task) {
        self.tasks.insert(label.clone(), value);
        self.map_symbol_type.insert(label, TASK_TYPE.into());
    }

    pub fn add_method(&mut self, label: String, value: Method) -> Result<(), LRuntimeError> {
        match self.tasks.get_mut(&value.task_label) {
            None => Err(lerror!(
                "DomainEnv::add_method",
                format!(
                    "Cannot add method {} because task {} does not exist.",
                    label, value.task_label
                )
            )),
            Some(task) => {
                task.methods.push(label.clone());
                self.methods.insert(label.clone(), value);
                self.map_symbol_type.insert(label, METHOD_TYPE.into());
                Ok(())
            }
        }
    }

    pub fn add_state_function(&mut self, label: String, value: StateFunction) {
        self.state_functions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label, STATE_FUNCTION_TYPE.into());
    }

    pub fn add_action(&mut self, label: String, value: Action) {
        self.actions.insert(label.clone(), value);
        self.map_symbol_type.insert(label, ACTION_TYPE.into());
    }

    pub fn add_action_sample_fn(
        &mut self,
        label: String,
        value: LValue,
    ) -> Result<(), LRuntimeError> {
        match self.actions.get_mut(&label) {
            None => Err(lerror!(
                "add_action_sample_fn",
                format!("Action {} is not defined", label)
            )),
            Some(action) => {
                //println!("updating sim of {} with {}", label, value);
                action.sim = value;
                Ok(())
            }
        }
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.lambdas.insert(label.clone(), value);
        self.map_symbol_type.insert(label, LAMBDA_TYPE.into());
    }

    pub fn add_type(&mut self, t: String, p: Option<String>) {
        self.types.add_type(t, p);
    }
}

//List of symbols
impl DomainEnv {
    pub fn get_list_tasks(&self) -> LValue {
        self.tasks
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_methods(&self) -> LValue {
        self.methods
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_state_functions(&self) -> LValue {
        self.state_functions
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_actions(&self) -> LValue {
        self.actions
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_lambdas(&self) -> LValue {
        self.lambdas
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_map_symbol_type(&self) -> &HashMap<String, String> {
        &self.map_symbol_type
    }
    pub fn get_parents(&self, t: &str) -> Vec<String> {
        self.types.get_parents(t)
    }

    pub fn get_childs(&self, t: &str) -> Vec<String> {
        self.types.get_childs(t)
    }
}

//Displayer
impl DomainEnv {
    pub fn print_tasks(&self) -> String {
        let mut str = "*TASKS:\n".to_string();
        for (label, value) in &self.tasks {
            str.push_str(format!("\t-{}:\n{}\n", label, value).as_str())
        }
        str
    }

    pub fn print_methods(&self) -> String {
        let mut str = "*METHODS:\n".to_string();
        for (label, value) in &self.methods {
            str.push_str(format!("\t-{}:\n{}\n", label, value).as_str())
        }
        str
    }

    pub fn print_state_functions(&self) -> String {
        let mut str = "*STATE-FUNCTIONS:\n\n".to_string();
        for (label, value) in &self.state_functions {
            str.push_str(format!("\t-{}:\n{}\n\n", label, value).as_str())
        }
        str
    }

    pub fn print_actions(&self) -> String {
        let mut str = "*ACTIONS:\n".to_string();
        for (label, value) in &self.actions {
            str.push_str(format!("\t-{}:\n{}\n\n", label, value).as_str())
        }
        str
    }

    pub fn print_lambdas(&self) -> String {
        let mut str = "*LAMBDAS:\n".to_string();
        for (label, value) in &self.lambdas {
            str.push_str(format!("\t-{}:\n{}\n", label, value.format(0)).as_str())
        }
        str
    }
}

impl Display for DomainEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "*** Domain defined in RAE ***\n".to_string();

        str.push_str(format!("\n{}", self.print_tasks()).as_str());
        str.push_str(format!("\n{}", self.print_methods()).as_str());
        str.push_str(format!("\n{}", self.print_state_functions()).as_str());
        str.push_str(format!("\n{}", self.print_actions()).as_str());
        str.push_str(format!("\n{}", self.print_lambdas()).as_str());

        write!(f, "{}", str)
    }
}

impl DomainEnv {
    pub fn get_exec_env(&self) -> LEnvSymbols {
        let mut env = LEnvSymbols::default();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();
        let mut map_action_model: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            map_method_pre_conditions.insert(label.into(), method.lambda_pre_conditions.clone());
            map_method_score.insert(label.into(), method.lambda_score.clone());
            map_method_types.insert(label.into(), method.parameters.get_types_as_lvalue());
        }

        //Add all actions to env:
        for (label, action) in self.get_actions() {
            env.insert(label.clone(), action.exec.clone());
            map_action_model.insert(label.into(), action.sim.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.body.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        env.insert(RAE_ACTION_LIST.to_string(), self.get_list_actions());
        env.insert(RAE_METHOD_LIST.to_string(), self.get_list_methods());
        env.insert(RAE_TASK_LIST.to_string(), self.get_list_tasks());
        env.insert(
            RAE_STATE_FUNCTION_LIST.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(
            RAE_SYMBOL_TYPE.to_string(),
            self.get_map_symbol_type().into(),
        );
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(RAE_TASK_METHODS_MAP.to_string(), map_task_method.into());
        env.insert(RAE_METHOD_TYPES_MAP.to_string(), map_method_types.into());
        env.insert(
            RAE_METHOD_PRE_CONDITIONS_MAP.to_string(),
            map_method_pre_conditions.into(),
        );

        env.insert(RAE_METHOD_SCORE_MAP.to_string(), map_method_score.into());

        env.insert(RAE_ACTION_MODEL_MAP.to_string(), map_action_model.into());

        env
    }

    pub fn get_sim_env(&self) -> LEnvSymbols {
        let mut env = LEnvSymbols::default();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            map_method_pre_conditions.insert(label.into(), method.lambda_pre_conditions.clone());
            map_method_score.insert(label.into(), method.lambda_score.clone());
            map_method_types.insert(label.into(), method.parameters.get_types_as_lvalue());
        }

        //Add all actions to env:
        for (label, action) in self.get_actions() {
            env.insert(label.clone(), action.sim.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.body.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        env.insert(RAE_ACTION_LIST.to_string(), self.get_list_actions());
        env.insert(RAE_METHOD_LIST.to_string(), self.get_list_methods());
        env.insert(RAE_TASK_LIST.to_string(), self.get_list_tasks());
        env.insert(
            RAE_STATE_FUNCTION_LIST.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(
            RAE_SYMBOL_TYPE.to_string(),
            self.get_map_symbol_type().into(),
        );
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(RAE_TASK_METHODS_MAP.to_string(), map_task_method.into());
        env.insert(RAE_METHOD_TYPES_MAP.to_string(), map_method_types.into());

        env.insert(
            RAE_METHOD_PRE_CONDITIONS_MAP.to_string(),
            map_method_pre_conditions.into(),
        );

        env.insert(RAE_METHOD_SCORE_MAP.to_string(), map_method_score.into());

        env
    }

    pub fn get_type_hierarchy(&self) -> &TypeHierarchy {
        &self.types
    }
}

pub struct RAEEnv {
    pub job_receiver: Option<Receiver<Job>>,
    pub agenda: Agenda,
    pub state: RAEState,
    pub env: LEnv,
    pub domain_env: DomainEnv,
}

impl RAEEnv {
    pub async fn get_exec_env(&self) -> LEnv {
        let domain_exec_symbols: LEnvSymbols = self.domain_env.get_exec_env();
        //let domain_sim_env = self.domain_env.get_sim_env();
        let mut exec_env = self.env.clone();
        //let (sim_env, sim_ctxs) = init_simu_env(None).await;

        /*let mut ctx_rae_sim_interface = CtxRaeSimInterface::new(sim_env, sim_ctxs);
        ctx_rae_sim_interface.add_domain_sim(domain_sim_env);

        import(
            &mut exec_env,
            &mut exec_ctxs,
            ctx_rae_sim_interface,
            WithoutPrefix,
        )
        .await
        .expect("error loading ctx_rae_sim_interface");*/

        exec_env.set_new_top_symbols(domain_exec_symbols);
        exec_env
    }

    pub fn get_sim_env(&self) -> LEnv {
        let symbols: LEnvSymbols = self.domain_env.get_sim_env();
        let mut env = self.env.clone();
        env.set_new_top_symbols(symbols);
        env
    }
}

//pub const RAE_MAP_TYPE:&str = "rae-map-type";

impl RAEEnv {
    #[allow(clippy::field_reassign_with_default)]
    pub async fn new(job_receiver: Option<Receiver<Job>>) -> Self {
        let env = get_root_env().await;
        Self {
            job_receiver,
            agenda: Default::default(),
            state: Default::default(),
            env,
            domain_env: Default::default(),
        }
    }
}

impl RAEEnv {
    pub fn add_action(&mut self, label: String, value: Action) -> Result<(), LRuntimeError> {
        self.domain_env.add_action(label, value);

        Ok(())
    }

    pub fn add_action_sample_fn(
        &mut self,
        label: String,
        value: LValue,
    ) -> Result<(), LRuntimeError> {
        self.domain_env.add_action_sample_fn(label, value)
    }

    pub fn add_state_function(
        &mut self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LRuntimeError> {
        self.domain_env.add_state_function(label, value);

        Ok(())
    }

    pub fn add_task(
        &mut self,
        label: String,
        body: LValue,
        parameters: Parameters,
    ) -> Result<(), LRuntimeError> {
        self.domain_env
            .add_task(label.clone(), Task::new(label, body, parameters));

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_method(
        &mut self,
        method_label: String,
        task_label: String,
        parameters: Parameters,
        conds: LValue,
        score: LValue,
        body: LValue,
    ) -> Result<(), LRuntimeError> {
        let method = Method::new(task_label, parameters, conds, score, body);

        self.domain_env.add_method(method_label, method)?;

        Ok(())
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.domain_env.add_lambda(label, value);
    }

    pub fn get_methods_from_task(&self, task: &LValue) -> Result<LValue, LRuntimeError> {
        let label: String = task.try_into()?;
        match self.domain_env.get_tasks().get(&*label) {
            None => Ok(LValue::Nil),
            Some(task) => Ok(task.methods.clone().into()),
        }
    }

    pub fn add_type(&mut self, t: String, p: Option<String>) {
        self.domain_env.add_type(t, p)
    }

    pub fn get_parents(&self, t: &str) -> Vec<String> {
        self.domain_env.get_parents(t)
    }

    pub fn format_type_hierarchy(&self) -> String {
        self.domain_env.types.format_hierarchy()
    }
}
