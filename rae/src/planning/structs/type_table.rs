use crate::planning::structs::symbol_table::SymTable;
use crate::planning::structs::traits::FormatWithSymTable;
use crate::planning::union_find::NodeId;
use std::fmt::{Display, Formatter};

pub type TypeId = NodeId;

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

    pub fn get_type_id(&self, pat: impl ToString) -> TypeId {
        *self.inner.get(&pat.to_string()).unwrap()
    }

    /*pub fn is_basic_type(&self, type_id: &TypeId) -> bool {
        self.reverse.contains_key(type_id)
    }*/
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
    Lambda,
    Object,
    Other(TypeId),
    SubType(TypeId),
}

impl FormatWithSymTable for PlanningAtomType {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{}",
            match self {
                PlanningAtomType::SubType(t) => {
                    format!("subtype of {}", t.format_with_sym_table(st))
                }
                PlanningAtomType::Other(t) => t.format_with_sym_table(st),
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
                PlanningAtomType::Action => "action".to_string(),
                PlanningAtomType::StateFunction => "state-function".to_string(),
                PlanningAtomType::Method => "method".to_string(),
                PlanningAtomType::Task => "task".to_string(),
                PlanningAtomType::Timepoint => "timepoint".to_string(),
                PlanningAtomType::Int => "int".to_string(),
                PlanningAtomType::Float => "float".to_string(),
                PlanningAtomType::Bool => "bool".to_string(),
                PlanningAtomType::Symbol => "symbol".to_string(),
                PlanningAtomType::Function => "function".to_string(),
                PlanningAtomType::Lambda => "lambda".to_string(),
                PlanningAtomType::Object => "object".to_string(),
                PlanningAtomType::Other(t) => format!("type({})", t),
                PlanningAtomType::SubType(t) => format!("subtype({})", t),
            }
        )
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum AtomKind {
    Constant,
    Variable(VariableKind),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum VariableKind {
    Result,
    Parameter,
}

impl Display for AtomKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AtomKind::Constant => "constant",
                AtomKind::Variable(b) => {
                    match b {
                        VariableKind::Result => "result",
                        VariableKind::Parameter => "parameter",
                    }
                }
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub a_type: Option<PlanningAtomType>,
    pub kind: AtomKind,
}
impl FormatWithSymTable for AtomType {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{}{}",
            match self.a_type {
                Some(t) => t.format_with_sym_table(st),
                None => "untyped".to_string(),
            },
            self.kind
        )
    }
}
