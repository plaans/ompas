use crate::structs::chronicle::ChronicleTemplate;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::FormatWithSymTable;
use crate::structs::type_table::AtomType;
use im::{hashmap, HashMap, HashSet};
use ompas_rae_language::RAE_INSTANCE;
use ompas_rae_structs::rae_env::{DomainEnv, Parameters, StateFunction, TypeHierarchy};
use ompas_rae_structs::rae_state::RAEStateSnapshot;
use sompas_structs::lenv::LEnv;
use sompas_structs::lvalues::LValueS;
use std::fmt::{Display, Formatter};

pub mod atom;
pub mod chronicle;
pub mod condition;
pub mod constraint;
pub mod effect;
pub mod expression;
pub mod expression_chronicle;
pub mod forest;
pub mod interval;
pub mod lit;
pub mod partial_chronicle;
pub mod symbol_table;
pub mod traits;
pub mod transition;
pub mod type_table;

pub const START: &str = "start";
pub const END: &str = "end";
pub const PREZ: &str = "prez";
pub const RESULT: &str = "result";
pub const COND: &str = "cond";
pub const IF_TASK_PROTOTYPE: &str = "t_if";

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

#[derive(Clone)]
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

pub fn get_variables_of_type(
    variables: im::HashSet<AtomId>,
    symbol_table: &SymTable,
    atom_type: AtomType,
) -> HashSet<AtomId> {
    variables
        .iter()
        .filter(|var| {
            symbol_table
                .get_type_of(symbol_table.get_parent(var))
                .unwrap()
                == &atom_type
        })
        .cloned()
        .collect()
}

#[derive(Clone, Default)]
pub struct ConversionCollection {
    pub state_function: Vec<StateFunction>,
    pub tasks: Vec<Vec<AtomId>>,
    pub chronicle_templates: Vec<ChronicleTemplate>,
    pub local_tasks: TaskTypeMetaDataCollection,
    pub sym_table: SymTable,
}

#[derive(Default)]
pub struct Problem {
    pub actions: Vec<String>,
    pub tasks: Vec<String>,
    pub methods: Vec<String>,
    pub types: TypeHierarchy,
    pub state_functions: Vec<(String, Parameters)>,
    pub objects: Vec<(String, String)>,
    pub initial_state: RAEStateSnapshot,
    pub goal_tasks: Vec<LValueS>,
    pub cc: ConversionCollection,
}

impl From<&ConversionContext> for Problem {
    fn from(cc: &ConversionContext) -> Self {
        let actions = cc.domain.get_actions().keys().cloned().collect();
        let tasks = cc.domain.get_tasks().keys().cloned().collect();
        let methods = cc.domain.get_methods().keys().cloned().collect();
        let mut types = cc.domain.get_type_hierarchy().clone();
        let mut objects = vec![];

        {
            for (k, v) in &cc.state.instance.inner {
                if let LValueS::List(list_instance) = k {
                    assert_eq!(list_instance.len(), 2);
                    assert_eq!(LValueS::from(RAE_INSTANCE), list_instance[0]);
                    let _type: String = list_instance[1].to_string();
                    if types.get_id(&_type) == None {
                        types.add_type(_type.clone(), None)
                    }
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
            goal_tasks: vec![],
            cc: Default::default(),
        }
    }
}

impl ConversionCollection {
    pub fn new(
        templates: Vec<ChronicleTemplate>,
        tasks: Vec<Vec<AtomId>>,
        sym_table: SymTable,
    ) -> Self {
        Self {
            state_function: vec![],
            tasks,
            chronicle_templates: templates,
            local_tasks: Default::default(),
            sym_table,
        }
    }
}

impl Display for ConversionCollection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();

        str.push_str("# DOMAIN:\n\n");

        //actions
        str.push_str("# TEMPLATES: \n\n");
        for (id, template) in self.chronicle_templates.iter().enumerate() {
            str.push_str(
                format!(
                    "#{}\n\
                    {}\n\n\n",
                    id,
                    template.format(&self.sym_table, true)
                )
                .as_str(),
            );
        }

        //tasks
        str.push_str("# TASKS: \n\n");
        for task in &self.tasks {
            str.push_str(format!("{}\n\n\n", task.format(&self.sym_table, true)).as_str());
        }

        //str.push_str(self.sym_table.to_string().as_str());

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
