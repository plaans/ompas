use crate::model::chronicle::{Chronicle, Instantiation};
use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::ompas::manager::planning::get_var_as_cst;
use crate::planning::planner::encoding::PlannerProblem;
use crate::planning::planner::result::PlanResult;

pub fn instantiate_chronicles(
    pp: &PlannerProblem,
    pr: &PlanResult,
    table: &ActingVarRefTable,
) -> Vec<Chronicle> {
    let mut instances = vec![];
    let ass = &pr.ass;
    let model = &pr.fp.model;
    let st = pp.st.clone();
    for instance in &pp.instances {
        let chronicle = &instance.instantiated_chronicle;
        let mut instantiations = vec![];
        for var in &chronicle.variables {
            let cst = get_var_as_cst(table, ass, model, var);
            let value = st.new_cst(cst);
            instantiations.push(Instantiation::new(*var, value));
        }

        let instantiated = chronicle.instantiate(instantiations);

        instances.push(instantiated)
    }

    instances
}
