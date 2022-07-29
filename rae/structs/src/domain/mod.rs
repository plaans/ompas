use crate::context::{
    ACTION_TYPE, LAMBDA_TYPE, METHOD_TYPE, RAE_ACTION_LIST, RAE_ACTION_MODEL_MAP, RAE_METHOD_LIST,
    RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP,
    RAE_STATE_FUNCTION_LIST, RAE_SYMBOL_TYPE, RAE_TASK_LIST, RAE_TASK_METHODS_MAP,
    STATE_FUNCTION_TYPE, TASK_TYPE,
};
use crate::domain::action::Action;
use crate::domain::method::Method;
use crate::domain::state_function::StateFunction;
use crate::domain::task::Task;
use crate::domain::type_hierarchy::TypeHierarchy;
use im::HashMap;
use sompas_structs::lenv::LEnvSymbols;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub mod _type;
pub mod action;
pub mod method;
pub mod parameters;
pub mod state_function;
pub mod task;
pub mod type_hierarchy;

#[derive(Default, Debug, Clone)]
pub struct RAEDomain {
    pub tasks: HashMap<String, Task>,
    pub methods: HashMap<String, Method>,
    pub state_functions: HashMap<String, StateFunction>,
    pub actions: HashMap<String, Action>,
    pub lambdas: HashMap<String, LValue>,
    pub types: TypeHierarchy,
    pub map_symbol_type: HashMap<String, String>,
}

impl RAEDomain {
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
impl RAEDomain {
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
impl RAEDomain {
    pub fn add_task(&mut self, label: String, value: Task) {
        self.tasks.insert(label.clone(), value);
        self.map_symbol_type.insert(label, TASK_TYPE.into());
    }

    pub fn add_method(&mut self, label: String, value: Method) -> Result<(), LRuntimeError> {
        match self.tasks.get_mut(&value.task_label) {
            None => Err(lruntimeerror!(
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
            None => Err(lruntimeerror!(
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
impl RAEDomain {
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
impl RAEDomain {
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

impl Display for RAEDomain {
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

impl RAEDomain {
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