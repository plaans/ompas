use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::method::Method;
use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use im::HashMap;
use ompas_language::*;
use sompas_structs::lenv::LEnvSymbols;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub mod command;
pub mod method;
pub mod model;
pub mod parameters;
pub mod state_function;
pub mod task;

#[derive(Default, Debug, Clone)]
pub struct OMPASDomain {
    pub tasks: HashMap<String, Task>,
    pub methods: HashMap<String, Method>,
    pub state_functions: HashMap<String, StateFunction>,
    pub commands: HashMap<String, Command>,
    pub lambdas: HashMap<String, LValue>,
    pub map_symbol_type: HashMap<String, String>,
    pub env: LEnvSymbols,
}

impl OMPASDomain {
    pub fn get_element_description(&self, label: &str) -> String {
        match self.map_symbol_type.get(label) {
            None => format!("Keyword {} is not defined in the domain.", label),
            Some(s) => match s.as_str() {
                TASK_TYPE => self.tasks.get(label).unwrap().to_string(),
                METHOD_TYPE => self.methods.get(label).unwrap().to_string(),
                STATE_FUNCTION_TYPE => self.state_functions.get(label).unwrap().to_string(),
                LAMBDA_TYPE => self.lambdas.get(label).unwrap().to_string(),
                ACTION_TYPE => self.commands.get(label).unwrap().to_string(),
                _ => panic!("There should no other type of symbol_type"),
            },
        }
    }

    pub fn is_task(&self, label: &str) -> bool {
        self.tasks.contains_key(label)
    }

    pub fn is_method(&self, label: &str) -> bool {
        self.methods.contains_key(label)
    }

    pub fn is_command(&self, label: &str) -> bool {
        self.commands.contains_key(label)
    }

    //Getters
    pub fn get_tasks(&self) -> &HashMap<String, Task> {
        &self.tasks
    }

    pub fn get_methods(&self) -> &HashMap<String, Method> {
        &self.methods
    }

    pub fn get_state_functions(&self) -> &HashMap<String, StateFunction> {
        &self.state_functions
    }

    pub fn get_commands(&self) -> &HashMap<String, Command> {
        &self.commands
    }

    pub fn get_lambdas(&self) -> &HashMap<String, LValue> {
        &self.lambdas
    }

    //Adders
    pub fn add_task(&mut self, label: String, task: Task) -> Result<(), LRuntimeError> {
        self.tasks.insert(label.clone(), task);
        self.map_symbol_type.insert(label, TASK_TYPE.into());
        Ok(())
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
                task.add_method(label.clone());
                self.methods.insert(label.clone(), value);
                self.map_symbol_type.insert(label, METHOD_TYPE.into());
                Ok(())
            }
        }
    }

    pub fn add_state_function(
        &mut self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LRuntimeError> {
        self.state_functions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label, STATE_FUNCTION_TYPE.into());
        Ok(())
    }

    pub fn add_command(&mut self, label: String, value: Command) -> Result<(), LRuntimeError> {
        self.commands.insert(label.clone(), value);
        self.map_symbol_type.insert(label, ACTION_TYPE.into());
        Ok(())
    }

    pub fn add_command_model(
        &mut self,
        label: String,
        value: LValue,
        kind: ModelKind,
    ) -> Result<(), LRuntimeError> {
        match self.commands.get_mut(&label) {
            None => Err(lruntimeerror!(
                "add_action_sample_fn",
                format!("Action {} is not defined", label)
            )),
            Some(action) => {
                //println!("updating sim of {} with {}", label, value);
                action.set_model(value, kind);
                Ok(())
            }
        }
    }

    pub fn add_task_model(
        &mut self,
        label: String,
        value: LValue,
        kind: ModelKind,
    ) -> Result<(), LRuntimeError> {
        match self.tasks.get_mut(&label) {
            None => Err(lruntimeerror!(
                "add_action_sample_fn",
                format!("Action {} is not defined", label)
            )),
            Some(task) => {
                //println!("updating sim of {} with {}", label, value);
                task.set_model(value, kind);
                Ok(())
            }
        }
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.lambdas.insert(label.clone(), value);
        self.map_symbol_type.insert(label, LAMBDA_TYPE.into());
    }

    pub fn add_env(&mut self, label: String, value: LValue) {
        self.env.insert(label, value);
    }

    /*pub async fn add_type(&mut self, t: String, p: Option<String>) {
        let parents: Vec<TypeId> = vec![self
            .lattice
            .get_type_id(match p {
                Some(p) => p.to_string(),
                None => TYPE_OBJECT.to_string(),
            })
            .await
            .unwrap()];
        self.lattice.add_type(t, parents).await;
    }*/

    //List of symbols
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

    pub fn get_list_commands(&self) -> LValue {
        self.commands
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

    /*pub async fn get_parents(&self, t: &str) -> Vec<String> {
        let id: &TypeId = &self.lattice.get_type_id(t).await.unwrap();
        let mut parents: Vec<TypeId> = self.lattice.get_parents(id).await;

        let mut new_parents = vec![];

        for p in parents.drain(..) {
            new_parents.push(self.lattice.format_type(&p).await)
        }

        new_parents
    }*/

    /*pub async fn get_childs(&self, t: &str) -> Vec<String> {
        let id = &self.lattice.get_type_id(t).await.unwrap();
        let mut childs: Vec<TypeId> = self.lattice.get_all_childs(id).await;

        let mut new_childs = vec![];

        for p in childs.drain(..) {
            new_childs.push(self.lattice.format_type(&p).await)
        }

        new_childs
    }*/

    //Displayers
    pub fn print_tasks(&self) -> String {
        let mut str = "*TASKS:\n".to_string();
        for (label, value) in self.tasks.iter() {
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

    pub fn print_commands(&self) -> String {
        let mut str = "*COMMANDS:\n".to_string();
        for (label, value) in &self.commands {
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

    pub fn get_exec_env(&self) -> LEnvSymbols {
        let mut env = self.env.clone();
        /*let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();
        let mut map_action_model: HashMap<LValue, LValue> = Default::default();*/

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            //map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.to_string(), method.lambda_body.clone());
            /*map_method_pre_conditions.insert(
                label.to_string().into(),
                method.lambda_pre_conditions.clone(),
            );
            map_method_score.insert(label.to_string().into(), method.lambda_score.clone());
            map_method_types.insert(
                label.to_string().into(),
                method.parameters.get_types_as_lvalue(),
            );*/
        }

        //Add all actions to env:
        for (label, action) in self.get_commands() {
            env.insert(label.clone(), action.get_body().clone());
            /*map_action_model.insert(
                label.into(),
                action.get_model(&ModelKind::PlantModel).unwrap().clone(),
            );*/
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.body.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        /*env.insert(__COMMANDS_LIST__.to_string(), self.get_list_commands());
        env.insert(__METHODS_LIST__.to_string(), self.get_list_methods());
        env.insert(__TASKS_LIST__.to_string(), self.get_list_tasks());
        env.insert(
            __STATE_FUNCTION_LIST__.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(
            __SYMBOL_TYPE__.to_string(),
            self.get_map_symbol_type().into(),
        );
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(__TASKS_METHODS_MAP__.to_string(), map_task_method.into());
        env.insert(__METHOD_TYPES_MAP__.to_string(), map_method_types.into());
        env.insert(
            __METHOD_PRE_CONDITIONS_MAP__.to_string(),
            map_method_pre_conditions.into(),
        );

        env.insert(__METHOD_SCORE_MAP__.to_string(), map_method_score.into());

        env.insert(__COMMAND_MODEL_MAP__.to_string(), map_action_model.into());*/

        env
    }

    pub fn get_convert_env(&self) -> LEnvSymbols {
        let mut env = self.env.clone();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        /*let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();*/

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            /*map_method_pre_conditions.insert(
                label.to_string().into(),
                method.lambda_pre_conditions.clone(),
            );
            map_method_score.insert(label.to_string().into(), method.lambda_score.clone());
            map_method_types.insert(
                label.to_string().into(),
                method.parameters.get_types_as_lvalue(),
            );*/
        }

        //Add all actions to env:
        for (label, action) in self.get_commands() {
            env.insert(
                label.clone(),
                action.get_model(&ModelKind::PlanModel).unwrap().clone(),
            );
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.body.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        /*env.insert(__COMMANDS_LIST__.to_string(), self.get_list_commands());
        env.insert(__METHODS_LIST__.to_string(), self.get_list_methods());
        env.insert(__TASKS_LIST__.to_string(), self.get_list_tasks());
        env.insert(
            __STATE_FUNCTION_LIST__.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(
            __SYMBOL_TYPE__.to_string(),
            self.get_map_symbol_type().into(),
        );
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(__TASKS_METHODS_MAP__.to_string(), map_task_method.into());
        env.insert(__METHOD_TYPES_MAP__.to_string(), map_method_types.into());

        env.insert(
            __METHOD_PRE_CONDITIONS_MAP__.to_string(),
            map_method_pre_conditions.into(),
        );

        env.insert(__METHOD_SCORE_MAP__.to_string(), map_method_score.into());*/

        env
    }
}

impl Display for OMPASDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "*** Domain defined in RAE ***\n".to_string();

        str.push_str(format!("\n{}", self.print_tasks()).as_str());
        str.push_str(format!("\n{}", self.print_methods()).as_str());
        str.push_str(format!("\n{}", self.print_state_functions()).as_str());
        str.push_str(format!("\n{}", self.print_commands()).as_str());
        str.push_str(format!("\n{}", self.print_lambdas()).as_str());

        write!(f, "{}", str)
    }
}
