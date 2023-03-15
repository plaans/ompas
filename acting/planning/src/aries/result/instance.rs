use crate::aries::result::PlanResult;
use aries_model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries_model::lang::Variable;
use aries_model::Model;
use aries_planning::chronicles::VarLabel;
use ompas_structs::acting_manager::operational_model::ActingModel;
use ompas_structs::acting_manager::planner_manager::{
    get_var_as_cst, BindingPlanner, RefBindingPlanner,
};
use ompas_structs::conversion::chronicle::Instantiation;
use ompas_structs::planning::instance::ChronicleInstance;
use ompas_structs::planning::problem::PlanningProblem;
use ompas_structs::sym_table::domain::cst::Cst;
use ompas_structs::sym_table::VarId;
use std::sync::Arc;

pub async fn instantiate_chronicles(
    pp: &PlanningProblem,
    pr: &PlanResult,
    bindings: &RefBindingPlanner,
) -> Vec<ChronicleInstance> {
    let bindings = bindings.inner.read().await;

    let mut instances = vec![];
    let ass = &pr.ass;
    let model = &pr.fp.model;
    let st = pp.st.clone();
    for instance in &pp.instance.instances {
        let chronicle = instance.om.chronicle.as_ref().unwrap();
        let mut instantiations = vec![];
        for var in &chronicle.variables {
            let cst = get_var_as_cst(&bindings, ass, model, var);
            let value = st.new_cst(cst);
            instantiations.push(Instantiation::new(*var, value));
        }

        let instantiated = chronicle.instantiate(instantiations);
        let om = ActingModel {
            lv: instance.om.lv.clone(),
            lv_om: instance.om.lv_om.clone(),
            lv_expanded: instance.om.lv_expanded.clone(),
            chronicle: Some(instantiated),
        };
        let instance = ChronicleInstance {
            generated: true,
            origin: instance.origin,
            om,
            pr: instance.pr.clone(),
        };

        instances.push(instance)
    }

    instances
}
