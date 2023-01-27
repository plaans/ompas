use crate::planning::domain::PlanningDomain;
use crate::planning::instance::PlanningInstance;
use crate::sym_table::r#ref::RefSymTable;

pub struct PlanningProblem {
    pub domain: PlanningDomain,
    pub instance: PlanningInstance,
    pub st: RefSymTable,
}
