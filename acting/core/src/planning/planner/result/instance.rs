use crate::model::chronicle::{Chronicle, Instantiation};
use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::planning::get_var_as_cst;
use crate::planning::planner::result::PlanResult;

pub fn instantiate_chronicles(pr: &PlanResult) -> Vec<Chronicle> {
    let mut instances = vec![];
    let PlanResult { ass, fp, pp, table } = pr;

    let model = &fp.model;
    let st = pp.st.clone();
    for chronicle in pp.instances.iter().filter_map(|c| {
        if get_var_as_cst(table, ass, model, c.instantiated_chronicle.get_presence())
            == Cst::Bool(true)
        {
            Some(&c.instantiated_chronicle)
        } else {
            None
        }
    }) {
        let mut instantiations = vec![];
        for var in &chronicle.variables {
            let cst = get_var_as_cst(table, ass, model, *var);
            let value = st.new_cst(cst);
            instantiations.push(Instantiation::new(*var, value));
        }

        let instantiated = chronicle.clone().instantiate(instantiations);

        instances.push(instantiated)
    }

    instances
}
