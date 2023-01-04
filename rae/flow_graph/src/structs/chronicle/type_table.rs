/*use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::sym_table::forest::NodeId;
use std::fmt::{Display, Formatter};

pub type TypeId = NodeId;
const COMMAND: &str = "command";
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
const PRESENCE: &str = "presence";
//const LOCAL: &str = "local";
//const PARAMETER: &str = "parameter";
//const CONSTANT: &str = "constant";
const UNTYPED: &str = "untyped";
const ROOT_TYPE: &str = "root-type";
const HANDLE: &str = "handle";

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AtomType {
    RootType,
    Untyped,
    Command,
    StateFunction,
    Method,
    Task,
    Timepoint,
    Presence,
    Handle,
    Int,
    Float,
    Bool,
    Symbol,
    Function,
    //Lambda,
    Object,
    Custom(TypeId),
    SubType(TypeId),
}

impl Default for AtomType {
    fn default() -> Self {
        Self::Untyped
    }
}

impl FormatWithSymTable for AtomType {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            AtomType::Custom(t) => t.format(st, sym_version),
            pat => pat.to_string(),
        }
    }
}
impl Display for AtomType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AtomType::Command => COMMAND.to_string(),
                AtomType::StateFunction => STATE_FUNCTION.to_string(),
                AtomType::Method => METHOD.to_string(),
                AtomType::Task => TASK.to_string(),
                AtomType::Timepoint => TIMEPOINT.to_string(),
                AtomType::Int => INT.to_string(),
                AtomType::Float => FLOAT.to_string(),
                AtomType::Bool => BOOL.to_string(),
                AtomType::Symbol => SYMBOL.to_string(),
                AtomType::Function => FUNCTION.to_string(),
                //PlanningAtomType::Lambda => LAMBDA.to_string(),
                AtomType::Object => OBJECT.to_string(),
                AtomType::Custom(t) => format!("{}", t),
                //PlanningAtomType::SubType(t) =>
                AtomType::Presence => PRESENCE.to_string(),
                AtomType::Untyped => UNTYPED.to_string(),
                AtomType::RootType => ROOT_TYPE.to_string(),
                AtomType::SubType(t) => format!("subtype({})", t),
                AtomType::Handle => HANDLE.to_string(),
            }
        )
    }
}

impl TypeTable {
    pub fn try_get_from_str(&self, value: &str) -> Option<AtomType> {
        match value {
            COMMAND => Some(AtomType::Command),
            STATE_FUNCTION => Some(AtomType::StateFunction),
            METHOD => Some(AtomType::Method),
            TASK => Some(AtomType::Task),
            SYMBOL => Some(AtomType::Symbol),
            FUNCTION => Some(AtomType::Function),
            OBJECT => Some(AtomType::Object),
            PRESENCE => Some(AtomType::Presence),
            other => self.get_type_id(other).map(|t| AtomType::Custom(*t)),
        }
    }
}

impl FormatWithSymTable for Option<AtomType> {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            Some(t) => t.format(st, sym_version),
            None => UNTYPED.to_string(),
        }
    }
}
*/
