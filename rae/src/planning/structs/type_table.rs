use crate::planning::structs::symbol_table::SymTable;
use crate::planning::structs::traits::FormatWithSymTable;
use crate::planning::union_find::NodeId;
use std::fmt::{Display, Formatter};

pub type TypeId = NodeId;

const ACTION: &str = "action";
const STATE_FUNCTION: &str = "state-function";
const METHOD: &str = "method";
const TASK: &str = "task";
const TIMEPOINT: &str = "timepoint";
const INT: &str = "int";
const FLOAT: &str = "float";
const BOOL: &str = "bool";
const SYMBOL: &str = "symbol";
const FUNCTION: &str = "function";
//const LAMBDA: &str = "lambda";
const OBJECT: &str = "object";
const LOCAL: &str = "local";
const PARAMETER: &str = "parameter";
const CONSTANT: &str = "constant";
const UNTYPED: &str = "untyped";

#[derive(Clone, Default)]
pub struct TypeTable {
    inner: im::HashMap<String, TypeId>,
    reverse: im::HashMap<TypeId, String>,
}

impl TypeTable {
    pub fn add_type(&mut self, pat: impl ToString, type_id: TypeId) {
        self.inner.insert(pat.to_string(), type_id);
        self.reverse.insert(type_id, pat.to_string());
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<&String> {
        self.reverse.get(type_id)
    }

    pub fn get_type_id(&self, pat: impl ToString) -> Option<&TypeId> {
        self.inner.get(&pat.to_string())
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub enum PlanningAtomType {
    Action,
    StateFunction,
    Method,
    Task,
    Timepoint,
    Int,
    Float,
    Bool,
    Symbol,
    Function,
    //Lambda,
    Object,
    Other(TypeId),
    SubType(TypeId),
}

impl FormatWithSymTable for PlanningAtomType {
    fn format_with_sym_table(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{}",
            match self {
                PlanningAtomType::SubType(t) => {
                    format!("subtype of {}", t.format_with_sym_table(st, sym_version))
                }
                PlanningAtomType::Other(t) => t.format_with_sym_table(st, sym_version),
                pat => pat.to_string(),
            }
        )
    }
}
impl Display for PlanningAtomType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PlanningAtomType::Action => ACTION.to_string(),
                PlanningAtomType::StateFunction => STATE_FUNCTION.to_string(),
                PlanningAtomType::Method => METHOD.to_string(),
                PlanningAtomType::Task => TASK.to_string(),
                PlanningAtomType::Timepoint => TIMEPOINT.to_string(),
                PlanningAtomType::Int => INT.to_string(),
                PlanningAtomType::Float => FLOAT.to_string(),
                PlanningAtomType::Bool => BOOL.to_string(),
                PlanningAtomType::Symbol => SYMBOL.to_string(),
                PlanningAtomType::Function => FUNCTION.to_string(),
                //PlanningAtomType::Lambda => LAMBDA.to_string(),
                PlanningAtomType::Object => OBJECT.to_string(),
                PlanningAtomType::Other(t) => format!("type({})", t),
                PlanningAtomType::SubType(t) => format!("subtype({})", t),
            }
        )
    }
}

impl TypeTable {
    pub fn try_get_from_str(&self, value: &str) -> Option<PlanningAtomType> {
        match value {
            ACTION => Some(PlanningAtomType::Action),
            STATE_FUNCTION => Some(PlanningAtomType::StateFunction),
            METHOD => Some(PlanningAtomType::Method),
            TASK => Some(PlanningAtomType::Task),
            TIMEPOINT => Some(PlanningAtomType::Timepoint),
            INT => Some(PlanningAtomType::Int),
            FLOAT => Some(PlanningAtomType::Float),
            BOOL => Some(PlanningAtomType::Bool),
            SYMBOL => Some(PlanningAtomType::Symbol),
            FUNCTION => Some(PlanningAtomType::Function),
            //LAMBDA => Some(PlanningAtomType::Lambda),
            OBJECT => Some(PlanningAtomType::Object),
            other => match self.get_type_id(other) {
                Some(t) => Some(PlanningAtomType::Other(*t)),
                None => None,
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum AtomKind {
    Constant,
    Variable(VariableKind),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum VariableKind {
    Local,
    Parameter,
}

impl Display for AtomKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AtomKind::Constant => CONSTANT,
                AtomKind::Variable(b) => {
                    match b {
                        VariableKind::Local => LOCAL,
                        VariableKind::Parameter => PARAMETER,
                    }
                }
            }
        )
    }
}

impl FormatWithSymTable for Option<PlanningAtomType> {
    fn format_with_sym_table(&self, st: &SymTable, sym_version: bool) -> String {
        match self {
            Some(t) => t.format_with_sym_table(st, sym_version),
            None => UNTYPED.to_string(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub a_type: Option<PlanningAtomType>,
    pub kind: AtomKind,
}
impl FormatWithSymTable for AtomType {
    fn format_with_sym_table(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} {}",
            self.kind,
            self.a_type.format_with_sym_table(st, sym_version)
        )
    }
}
