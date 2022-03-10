use crate::context::rae_env::{DomainEnv, Parameters, StateFunction};
use crate::context::rae_state::RAEStateSnapshot;
use crate::module::rae_exec::RAE_INSTANCE;
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::chronicle::Chronicle;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::FormatWithSymTable;
use im::{hashmap, HashMap, HashSet};
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lvalues::LValueS;
use std::fmt::{Display, Formatter};

pub mod atom;
pub mod chronicle;
pub mod condition;
pub mod constraint;
pub mod effect;
pub mod expression;
pub mod interval;
pub mod lit;
pub mod symbol_table;
pub mod traits;
pub mod transition;

pub const IF_TASK_PROTOTYPE: &str = "t_if_task";

#[derive(Default, Copy, Clone)]
pub struct TaskTypeMetaData {
    number: usize,
    label_prototype: &'static str,
}

impl TaskTypeMetaData {
    pub fn new(label_prototype: &'static str) -> Self {
        Self {
            number: 0,
            label_prototype,
        }
    }

    pub fn get_number(&self) -> usize {
        self.number
    }
    pub fn new_label(&mut self) -> String {
        let r = format!("{}_{}", self.label_prototype, self.number);
        self.number += 1;
        r
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum TaskType {
    IfTask,
}

impl Display for TaskType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TaskType::IfTask => "IfTask",
        };
        write!(f, "{}", str)
    }
}

pub struct TaskTypeMetaDataCollection {
    inner: HashMap<TaskType, TaskTypeMetaData>,
}

impl Default for TaskTypeMetaDataCollection {
    fn default() -> Self {
        Self {
            inner: hashmap! {
                TaskType::IfTask => TaskTypeMetaData::new(IF_TASK_PROTOTYPE)
            },
        }
    }
}

impl TaskTypeMetaDataCollection {
    pub fn get_number_of(&self, local_task_type: TaskType) -> usize {
        self.inner
            .get(&local_task_type)
            .unwrap_or_else(|| {
                panic!(
                    "{} type not initialzed in LocalTaskMetaData",
                    local_task_type
                )
            })
            .get_number()
    }

    pub fn new_label(&mut self, local_task_type: TaskType) -> String {
        self.inner
            .get_mut(&local_task_type)
            .unwrap_or_else(|| {
                panic!(
                    "{} type not initialzed in LocalTaskMetaData",
                    local_task_type
                )
            })
            .new_label()
    }
}

#[derive(Default, Clone)]
pub struct ConversionContext {
    pub domain: DomainEnv,
    pub env: LEnv,
    pub state: RAEStateSnapshot,
}

/*impl From<&RAEEnv> for ConversionContext {
    fn from(rae_env: &RAEEnv) -> Self {
        Self {
            domain: rae_env.domain_env.clone(),
            env: rae_env.env.clone(),
            state: rae_env.state.get_snapshot(),
        }
    }
}*/

/*impl From<RAEEnv> for ConversionContext {
    fn from(ctx: RAEEnv) -> Self {
        (&ctx).into()
    }
}*/

pub fn get_variables_of_type(
    variables: im::HashSet<AtomId>,
    symbol_table: &SymTable,
    atom_type: AtomType,
) -> HashSet<AtomId> {
    variables
        .iter()
        .filter(|var| {
            symbol_table
                .get_type_of(&symbol_table.get_parent(var))
                .unwrap()
                == &atom_type
        })
        .cloned()
        .collect()
}

#[derive(Default)]
pub struct ChronicleHierarchy {
    pub state_function: Vec<StateFunction>,
    pub tasks: Vec<Lit>,
    pub chronicle_templates: Vec<Chronicle>,
    pub local_tasks: TaskTypeMetaDataCollection,
    pub sym_table: SymTable,
    pub problem: Problem,
}

#[derive(Default)]
pub struct Problem {
    pub actions: Vec<String>,
    pub tasks: Vec<String>,
    pub methods: Vec<String>,
    pub types: Vec<String>,
    pub state_functions: Vec<(String, Parameters)>,
    pub objects: Vec<(String, String)>,
    pub initial_state: RAEStateSnapshot,
}

impl From<&ConversionContext> for Problem {
    fn from(cc: &ConversionContext) -> Self {
        let actions = cc.domain.get_actions().keys().cloned().collect();
        let tasks = cc.domain.get_tasks().keys().cloned().collect();
        let methods = cc.domain.get_methods().keys().cloned().collect();
        let mut types = vec![];
        let mut objects = vec![];

        {
            for (k, v) in &cc.state.instance.inner {
                if let LValueS::List(list_instance) = k {
                    assert_eq!(list_instance.len(), 2);
                    assert_eq!(LValueS::from(RAE_INSTANCE), list_instance[0]);
                    let _type: String = list_instance[1].to_string();
                    types.push(_type.clone());
                    if let LValueS::List(instance) = v {
                        for element in instance {
                            objects.push((element.to_string(), _type.clone()))
                        }
                    }
                }
            }
        };
        let state_functions = cc
            .domain
            .get_state_functions()
            .iter()
            .map(|(k, v)| (k.clone(), v.get_parameters().clone()))
            .collect();

        Self {
            actions,
            tasks,
            methods,
            types,
            state_functions,
            objects,
            initial_state: cc.state.clone(),
        }
    }
}

impl ChronicleHierarchy {
    pub fn new(templates: Vec<Chronicle>, tasks: Vec<Lit>, sym_table: SymTable) -> Self {
        Self {
            state_function: vec![],
            tasks,
            chronicle_templates: templates,
            local_tasks: Default::default(),
            sym_table,
            problem: Default::default(),
        }
    }
}

impl Display for ChronicleHierarchy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();

        str.push_str("# DOMAIN:\n");

        //actions
        str.push_str("# CHRONICLES: \n");
        for action in &self.chronicle_templates {
            str.push_str(format!("{}\n", action.format_with_sym_table(&self.sym_table)).as_str());
        }

        //tasks
        str.push_str("# TASKS: \n\n");
        for task in &self.tasks {
            str.push_str(format!("{}\n\n\n", task.format_with_sym_table(&self.sym_table)).as_str());
        }

        write!(f, "{}", str)
    }
}

/*
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

        //str.push_str(format!("FOREST:\n{} \n", st.symbols).as_str());

        str
    }
}*/
