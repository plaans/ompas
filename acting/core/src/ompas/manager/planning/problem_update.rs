use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::ompas::manager::acting::acting_var::PlanVarRef;
use crate::ompas::manager::acting::ActingProcessId;
use crate::ompas::manager::state::state_update_manager::StateUpdate;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::planning::planner::problem::ChronicleInstance;

pub enum PlannerUpdate {
    Plan,
    VarUpdate(VarUpdate),
    ProblemUpdate(ActingProcessId),
    StateUpdate(StateUpdate),
}

pub struct ExecutionProblem {
    pub(crate) state: WorldStateSnapshot,
    pub(crate) chronicles: Vec<ChronicleInstance>,
}

#[derive(Debug, Clone)]
pub struct VarUpdate {
    pub var_ref: PlanVarRef,
    pub value: Cst,
}

impl FormatWithSymTable for VarUpdate {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{{ var_id = {}, val = {}}}",
            self.var_ref.var_id().format(st, sym_version),
            self.value
        )
    }
}
