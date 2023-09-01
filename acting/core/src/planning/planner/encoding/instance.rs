use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::planning::planner::encoding::domain::read_chronicle;
use crate::planning::planner::problem::ChronicleInstance;
use aries::core::Lit;
use aries::model::lang::Atom as aAtom;
use aries_planning::chronicles::{ChronicleInstance as ACI, ChronicleOrigin, Container, Ctx};

pub fn generate_instances(
    ctx: &mut Ctx,
    table: &mut ActingVarRefTable,
    instances: &[ChronicleInstance],
) -> anyhow::Result<Vec<ACI>> {
    let mut new_instances: Vec<ACI> = vec![];

    for (id, instance) in instances.iter().enumerate() {
        let scope = match &instance.origin {
            ChronicleOrigin::Refinement { refined, .. } => {
                let task_id = refined.first().unwrap();
                new_instances[task_id.instance_id].chronicle.presence
            }
            ChronicleOrigin::Original => Lit::TRUE,
            _ => panic!(),
        };
        let template = read_chronicle(
            ctx,
            table,
            &instance.instantiated_chronicle,
            Container::Instance(id),
            Some(scope),
        )?;
        //Printer::print_chronicle(&template.chronicle, &ctx.model);

        new_instances.push(ACI {
            parameters: template
                .parameters
                .iter()
                .map(|v| aAtom::from(*v))
                .collect(),
            origin: instance.origin.clone(),
            chronicle: template.chronicle,
        });
    }
    Ok(new_instances)
}
