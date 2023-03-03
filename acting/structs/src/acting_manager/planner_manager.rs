use crate::acting_manager::process::plan_var::{AsCst, ExecutionVar, PlanVar};
use crate::planning::instance::ChronicleInstance;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::FormatWithSymTable;
use crate::sym_table::VarId;
use aries_model::extensions::SavedAssignment;
use aries_model::lang::{Atom, Variable};
use aries_planning::chronicles::FiniteProblem;
use std::fmt::Display;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct PlannerManager {
    pub(crate) plan_vars: Vec<PlanVar>,
    pub(crate) bindings: RefBindingPlanner,
}

impl PlannerManager {
    pub fn format_execution_var<T: Display + Clone + AsCst>(
        &self,
        execution_var: &ExecutionVar<T>,
    ) -> String {
        if let Some(val) = &execution_var.val {
            val.as_cst().to_string()
        } else if let Some(var_id) = &execution_var.plan_var_id {
            self.plan_vars[*var_id].to_string()
        } else {
            "".to_string()
        }
    }

    pub async fn clear(&mut self) {
        self.plan_vars.clear();
        self.bindings.inner.write().await.clear();
    }
}

#[derive(Default, Clone)]
pub struct RefBindingPlanner {
    pub inner: Arc<RwLock<BindingPlanner>>,
}

#[derive(Default)]
pub struct BindingPlanner {
    inner: im::HashMap<VarId, Variable>,
    reverse: im::HashMap<Variable, VarId>,
}

impl BindingPlanner {
    pub fn clear(&mut self) {
        self.inner.clear();
        self.reverse.clear();
    }

    pub fn add_binding(&mut self, id: &VarId, var: &Variable) {
        self.inner.insert(*id, *var);
        self.reverse.insert(*var, *id);
    }

    pub fn get_var(&self, id: &VarId) -> Option<&Variable> {
        self.inner.get(id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&VarId> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for BindingPlanner {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, id) in &self.reverse {
            str.push_str(
                format!("{:?} <- {}\n", Atom::from(*var), id.format(st, sym_version)).as_str(),
            )
        }
        str
    }
}

pub struct ActingPlanResult {
    pub instances: Vec<ChronicleInstance>,
    pub assignements: Arc<SavedAssignment>,
    pub finite_problem: FiniteProblem,
}
