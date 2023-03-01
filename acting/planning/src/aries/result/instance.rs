use crate::aries::result::PlanResult;
use crate::aries::BindingAriesAtoms;
use aries_model::extensions::{AssignmentExt, Shaped};
use aries_model::lang::Variable;
use ompas_structs::acting_manager::operational_model::ActingModel;
use ompas_structs::conversion::chronicle::Instantiation;
use ompas_structs::planning::instance::ChronicleInstance;
use ompas_structs::planning::problem::PlanningProblem;

pub fn instantiate_chronicles(
    pp: &PlanningProblem,
    pr: &PlanResult,
    bindings: &BindingAriesAtoms,
) -> Vec<ChronicleInstance> {
    let mut instances = vec![];
    let ass = &pr.ass;
    let model = &pr.fp.model;
    let st = pp.st.clone();
    for chronicle in &pp.instance.instances {
        let mut instantiations = vec![];
        for var in &chronicle.om.chronicle.variables {
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

        let instantiated = chronicle.om.chronicle.instantiate(instantiations);
        let om = ActingModel {
            lv: chronicle.om.lv.clone(),
            lv_om: chronicle.om.lv_om.clone(),
            lv_expanded: chronicle.om.lv_expanded.clone(),
            chronicle: instantiated,
        };
        let instance = ChronicleInstance {
            origin: chronicle.origin,
            om,
            pr: chronicle.pr.clone(),
        };

        instances.push(instance)
    }

    instances
}
