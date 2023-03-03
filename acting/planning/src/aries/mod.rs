pub mod instance;
pub mod problem_generation;
pub mod result;
pub mod solver;
pub mod template;
pub mod useful;

use crate::aries::instance::{create_initial_chronicle, generate_instances};
use anyhow::Result;
use aries_core::IntCst;
use aries_planning::chronicles;
use aries_planning::chronicles::TIME_SCALE;
use ompas_structs::acting_manager::planner_manager::RefBindingPlanner;
use ompas_structs::planning::problem::PlanningProblem;
//pub const FLOAT_SCALE: IntCst = TIME_SCALE;
/// Resolution of ms
pub const OMPAS_TIME_SCALE: IntCst = TIME_SCALE;
pub const FLOAT_SCALE: IntCst = OMPAS_TIME_SCALE;

pub async fn generate_chronicles(
    bindings: &RefBindingPlanner,
    problem: &PlanningProblem,
) -> Result<chronicles::Problem> {
    /*println!("# SYMBOL TABLE: \n{:?}", ctx.model.get_symbol_table());
    println!("{}", bindings.format(&problem.cc.sym_table, false));
    println!("initial chronicle: {:?}", init_ch.chronicle);*/

    /*for (i, t) in templates.iter().enumerate() {
        println!("template {}: {:?}", i, t.chronicle)
    }*/

    //let instant = Instant::now();

    let mut bindings = bindings.inner.write().await;
    let mut p = template::generate_templates(problem, &mut bindings)?;

    /*for template in &p.templates {
        Printer::print_chronicle(&template.chronicle, &p.context.model);
    }*/

    let init_ch = create_initial_chronicle(&problem, &mut p.context);

    //Printer::print_chronicle(&init_ch.chronicle, &p.context.model);

    p.chronicles.push(init_ch);

    let mut instances = generate_instances(&problem, &mut p.context, &mut bindings)?;

    p.chronicles.append(&mut instances);

    /*info!(
        "Generation of the planning problem: {:.3} ms",
        instant.elapsed().as_micros() as f64 / 1000.0
    );*/
    Ok(p)
}
