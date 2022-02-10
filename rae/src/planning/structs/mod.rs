use crate::context::rae_env::{DomainEnv, RAEEnv};
use crate::planning::structs::atom::AtomType;
use crate::planning::structs::chronicle::Chronicle;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::FormatWithSymTable;
use im::HashSet;
use ompas_lisp::core::structs::lenv::LEnv;

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

#[derive(Default)]
pub struct ConversionContext {
    pub domain: DomainEnv,
    pub env: LEnv,
}

impl From<&RAEEnv> for ConversionContext {
    fn from(rae_env: &RAEEnv) -> Self {
        Self {
            domain: rae_env.domain_env.clone(),
            env: rae_env.env.clone(),
        }
    }
}

impl From<RAEEnv> for ConversionContext {
    fn from(ctx: RAEEnv) -> Self {
        (&ctx).into()
    }
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
                .get_type(&symbol_table.get_parent(var))
                .unwrap()
                == &atom_type
        })
        .cloned()
        .collect()
}

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

        //str.push_str(format!("FOREST:\n{} \n", st.symbols).as_str());

        str
    }
}
