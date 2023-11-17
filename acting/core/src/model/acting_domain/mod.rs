use crate::model::acting_domain::acting_model_collection::ActingModelCollection;
use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::event::Event;
use crate::model::acting_domain::method::Method;
use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::parameters::{ParameterType, Parameters};
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use crate::model::add_domain_symbols;
use crate::model::sym_domain::basic_type::{TYPE_ID_INT, TYPE_INT};
use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::OMPAS_PRE_COMPUTE_MODELS;
use ompas_language::exec::resource::{MAX_Q, MAX_QUANTITY, QUANTITY};
use ompas_language::exec::state::INSTANCE;
use ompas_language::sym_table::{TYPE_OBJECT, TYPE_OBJECT_TYPE};
use ompas_language::*;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub mod acting_model_collection;
pub mod command;
pub mod event;
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
    pub init: LValue,
    pub events: HashMap<String, Event>,
    pub acting_model_collection: Option<ActingModelCollection>,
}

impl OMPASDomain {
    pub async fn init_acting_model_collection(
        &mut self,
        env: &LEnv,
        state: WorldStateSnapshot,
        st: &RefSymTable,
    ) {
        add_domain_symbols(&st, &self);
        let mut amc = ActingModelCollection::default();
        if OMPAS_PRE_COMPUTE_MODELS.get() {
            amc.pre_compute_acting_models(&self, env, state, st).await;
        }
        self.acting_model_collection = Some(amc)
    }

    pub fn init(&mut self, st: &RefSymTable) {
        self.add_state_function(
            QUANTITY.to_string(),
            StateFunction::new(
                QUANTITY.to_string(),
                Parameters::new(vec![(
                    Arc::new("?o".to_string()),
                    ParameterType::new(
                        TYPE_OBJECT.into(),
                        st.get_type_as_domain(TYPE_OBJECT).unwrap(),
                    ),
                )]),
                Domain::IntRange(0, MAX_QUANTITY),
                TYPE_INT.to_string(),
                None,
            ),
        )
        .unwrap();
        self.add_state_function(
            MAX_Q.to_string(),
            StateFunction::new(
                MAX_Q.to_string(),
                Parameters::new(vec![(
                    Arc::new("?o".to_string()),
                    ParameterType::new(
                        TYPE_OBJECT.into(),
                        st.get_type_as_domain(TYPE_OBJECT).unwrap(),
                    ),
                )]),
                Domain::Simple(TYPE_ID_INT),
                TYPE_INT.to_string(),
                None,
            ),
        )
        .unwrap();
        self.add_state_function(
            INSTANCE.to_string(),
            StateFunction::new(
                INSTANCE.to_string(),
                Parameters::new(vec![(
                    Arc::new("?o".to_string()),
                    ParameterType::new(
                        TYPE_OBJECT.into(),
                        st.get_type_as_domain(TYPE_OBJECT).unwrap(),
                    ),
                )]),
                st.get_type_as_domain(TYPE_OBJECT_TYPE).unwrap(),
                TYPE_OBJECT_TYPE.to_string(),
                None,
            ),
        )
        .unwrap();
    }

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
                task.add_method(&label);
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

    pub fn add_init(&mut self, body: LValue) {
        self.init = body;
    }

    pub fn get_init(&self) -> &LValue {
        &self.init
    }

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

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
        }

        //Add all methods to env:
        for (label, method) in self.get_methods() {
            env.insert(label.to_string(), method.lambda_body.clone());
        }

        //Add all actions to env:
        for (label, action) in self.get_commands() {
            env.insert(label.clone(), action.get_body().clone());
        }

        //Add all state functions to env:
        for (label, sf) in self.get_state_functions() {
            if let Some(body) = sf.get_body() {
                env.insert(label.clone(), body.clone())
            };
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        env
    }

    pub fn get_convert_env(&self) -> LEnvSymbols {
        let mut env = self.env.clone();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
        }

        //Add all actions to env:
        for (label, action) in self.get_commands() {
            env.insert(label.clone(), action.get_body().clone());
        }

        //Add all state functions to env:
        for (label, sf) in self.get_state_functions() {
            if let Some(body) = sf.get_body() {
                env.insert(label.clone(), body.clone());
            }
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

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
