use aries::model::extensions::{AssignmentExt, SavedAssignment};
use aries::model::lang::Variable;
use aries_planning::chronicles::{printer, FiniteProblem, Sub, Substitute};
use std::sync::Arc;

pub mod acting;
pub mod instance;
pub mod plan;

pub struct PlanResult {
    pub ass: Arc<SavedAssignment>,
    pub fp: Arc<FiniteProblem>,
}

pub fn print_chronicles(pr: &PlanResult) {
    let ass = &pr.ass;
    let problem = &pr.fp;
    let model = &problem.model;

    for chronicle in problem.chronicles.iter().filter_map(|ci| {
        if let Some(true) = ass.boolean_value_of(ci.chronicle.presence) {
            Some(&ci.chronicle)
        } else {
            None
        }
    }) {
        /*let mut sub = Sub::empty();

        for v in chronicle.variables() {
            if sub.contains(v) {
                // we already add this variable, ignore it
                continue;
            }

            let label = lbl_of_new(v, &pb.model);
            let fresh: Variable = match v {
                Variable::Bool(_) => pb.model.new_optional_bvar(prez_lit, label).into(),
                Variable::Int(i) => {
                    let (lb, ub) = pb.model.int_bounds(i);
                    pb.model.new_optional_ivar(lb, ub, prez_lit, label).into()
                }
                Variable::Fixed(f) => {
                    let (lb, ub) = pb.model.int_bounds(f.num);
                    pb.model
                        .new_optional_fvar(lb, ub, f.denom, prez_lit, label)
                        .into()
                }
                Variable::Sym(s) => pb.model.new_optional_sym_var(s.tpe, prez_lit, label).into(),
            };
            sub.add(v, fresh)?;
        }
        let chronicle = chronicle.substitute(&sub);*/

        printer::Printer::print_chronicle(&chronicle, model)
    }
}
