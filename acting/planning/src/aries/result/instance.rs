use crate::aries::result::PlanResult;
use aries_model::extensions::{AssignmentExt, Shaped};
use aries_model::lang::Variable;
use ompas_structs::acting_manager::operational_model::ActingModel;
use ompas_structs::acting_manager::planner_manager::RefBindingPlanner;
use ompas_structs::conversion::chronicle::Instantiation;
use ompas_structs::planning::instance::ChronicleInstance;
use ompas_structs::planning::problem::PlanningProblem;

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
            let value = match bindings.get_var(var).unwrap() {
                Variable::Bool(b) => st.new_bool(ass.boolean_value_of(*b).unwrap()),
                Variable::Int(i) => {
                    let value = ass.var_domain(*i).lb;
                    st.new_int(value as i64)
                }
                Variable::Fixed(f) => {
                    let value = ass.f_domain(*f).lb();
                    st.new_float(value as f64)
                }
                Variable::Sym(s) => {
                    let sym = ass.sym_domain_of(*s).into_singleton().unwrap();
                    let value = model.get_symbol(sym);
                    st.new_symbol(value)
                }
            };
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
