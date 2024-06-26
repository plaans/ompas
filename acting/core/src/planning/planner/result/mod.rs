use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::planning::planner::encoding::PlannerProblem;
use aries::model::extensions::{AssignmentExt, SavedAssignment};
use aries_planning::chronicles::{printer, FiniteProblem};
use std::sync::Arc;

pub mod acting;
pub mod instance;
pub mod plan;

pub struct PlanResult {
    pub ass: Arc<SavedAssignment>,
    pub fp: Arc<FiniteProblem>,
    pub pp: Arc<PlannerProblem>,
    pub table: Arc<ActingVarRefTable>,
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
        printer::Printer::print_chronicle(chronicle, model)
    }
}
