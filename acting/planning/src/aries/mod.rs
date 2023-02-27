pub mod instance;
pub mod problem_generation;
pub mod result;
pub mod solver;
pub mod template;
pub mod useful;

use crate::aries::instance::{create_initial_chronicle, generate_instances};
use anyhow::Result;
use aries_core::IntCst;
use aries_model::lang::{Atom as aAtom, Variable};
use aries_planning::chronicles;
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::TIME_SCALE;
use ompas_structs::planning::problem::PlanningProblem;
use ompas_structs::sym_table::r#ref::RefSymTable;
use ompas_structs::sym_table::r#trait::FormatWithSymTable;
use ompas_structs::sym_table::VarId;
//pub const FLOAT_SCALE: IntCst = TIME_SCALE;
/// Resolution of ms
pub const OMPAS_TIME_SCALE: IntCst = TIME_SCALE;
pub const FLOAT_SCALE: IntCst = OMPAS_TIME_SCALE;

pub fn generate_chronicles(
    problem: &PlanningProblem,
) -> Result<(chronicles::Problem, BindingAriesAtoms)> {
    /*println!("# SYMBOL TABLE: \n{:?}", ctx.model.get_symbol_table());
    println!("{}", bindings.format(&problem.cc.sym_table, false));
    println!("initial chronicle: {:?}", init_ch.chronicle);*/

    /*for (i, t) in templates.iter().enumerate() {
        println!("template {}: {:?}", i, t.chronicle)
    }*/

    //let instant = Instant::now();
    let mut bindings = BindingAriesAtoms::default();

    let mut p = template::generate_templates(problem, &mut bindings)?;

    for template in &p.templates {
        Printer::print_chronicle(&template.chronicle, &p.context.model);
    }

    let init_ch = create_initial_chronicle(&problem, &mut p.context);

    Printer::print_chronicle(&init_ch.chronicle, &p.context.model);

    p.chronicles.push(init_ch);

    let mut instances = generate_instances(&problem, &mut p.context, &mut bindings)?;

    p.chronicles.append(&mut instances);

    /*info!(
        "Generation of the planning problem: {:.3} ms",
        instant.elapsed().as_micros() as f64 / 1000.0
    );*/
    Ok((p, bindings))
}

#[derive(Default)]
pub struct BindingAriesAtoms {
    inner: im::HashMap<VarId, Variable>,
    reverse: im::HashMap<Variable, VarId>,
}

impl BindingAriesAtoms {
    pub fn add_binding(&mut self, id: &VarId, var: &Variable) {
        self.inner.insert(*id, *var);
        self.reverse.insert(*var, *id);
    }

    pub fn get_var(&self, id: &VarId) -> Option<&Variable> {
        self.inner.get(id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&VarId> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for BindingAriesAtoms {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, id) in &self.reverse {
            str.push_str(
                format!(
                    "{:?} <- {}\n",
                    aAtom::from(*var),
                    id.format(st, sym_version)
                )
                .as_str(),
            )
        }
        str
    }
}
