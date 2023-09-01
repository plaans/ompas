/*
pub async fn instantiate_chronicles(
    pp: &PlanningProblem,
    pr: &PlanResult,
    table: &mut ActingVarRefTable,
) -> Vec<ChronicleInstance> {
    let mut instances = vec![];
    let ass = &pr.ass;
    let model = &pr.fp.model;
    let st = pp.st.clone();
    for instance in &pp.instance.instances {
        let chronicle = instance.am.chronicle.as_ref().unwrap();
        let mut instantiations = vec![];
        for var in &chronicle.variables {
            let cst = get_var_as_cst(table, ass, model, var);
            let value = st.new_cst(cst);
            instantiations.push(Instantiation::new(*var, value));
        }

        let instantiated = chronicle.instantiate(instantiations);
        let om = ActingModel {
            lv: instance.am.lv.clone(),
            lv_om: instance.am.lv_om.clone(),
            lv_expanded: instance.am.lv_expanded.clone(),
            instantiations: vec![],
            chronicle: Some(instantiated),
        };
        let instance = ChronicleInstance {
            generated: true,
            origin: instance.origin.clone(),
            am: om,
            pr: instance.pr.clone(),
            refinement_label: instance.refinement_label,
        };

        instances.push(instance)
    }

    instances
}*/
