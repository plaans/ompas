use crate::acting_manager::process::process_ref::Label;
use crate::conversion::chronicle;
use crate::conversion::chronicle::subtask::SubTask;
use crate::conversion::chronicle::{Chronicle, ChronicleKind};
use crate::planning::om_binding::ActionBinding;
use crate::sym_table::domain::cst;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use sompas_structs::lvalue::LValue;

pub(crate) const ROOT: &str = "ROOT";

#[derive(Clone)]
pub struct ActingModel {
    pub lv: LValue,
    pub lv_om: LValue,
    pub lv_expanded: LValue,
    pub chronicle: Chronicle,
}

impl ActingModel {
    pub fn root(st: RefSymTable) -> Self {
        let chronicle = Chronicle::new(ROOT, ChronicleKind::Root, st);

        Self {
            lv: LValue::Nil,
            lv_om: LValue::Nil,
            lv_expanded: LValue::Nil,
            chronicle,
        }
    }

    pub fn add_subtask(&mut self, mut task: Vec<cst::Cst>) -> (VarId, VarId) {
        let interval = chronicle::interval::Interval::new(
            self.chronicle.st.new_timepoint(),
            self.chronicle.st.new_timepoint(),
        );

        let start = interval.get_start();
        let end = interval.get_end();

        let result = self.chronicle.st.new_result();

        let name: Vec<VarId> = task
            .drain(..)
            .map(|cst| self.chronicle.st.new_cst(cst))
            .collect();

        let n_subtask = self.chronicle.get_subtasks().len();
        let label = Label::Action(n_subtask);

        let subtask = SubTask {
            interval,
            name: name.clone(),
            result,
            label: Some(label),
        };

        let binding = ActionBinding {
            name,
            index: self.chronicle.get_subtasks().len(),
            interval,
        };

        self.chronicle.add_subtask(subtask);
        self.chronicle.bindings.add_binding(label, binding);

        (start, end)
    }
}
