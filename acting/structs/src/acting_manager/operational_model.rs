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
    pub chronicle: Option<Chronicle>,
}

pub struct TaskRef {
    pub start: VarId,
    pub end: VarId,
    pub name: Vec<VarId>,
}

impl ActingModel {
    pub fn root(st: RefSymTable) -> Self {
        let chronicle = Chronicle::new(ROOT, ChronicleKind::Root, st);

        Self {
            lv: LValue::Nil,
            lv_om: LValue::Nil,
            lv_expanded: LValue::Nil,
            chronicle: Some(chronicle),
        }
    }

    pub fn add_subtask(&mut self, mut task: Vec<cst::Cst>) -> TaskRef {
        let chronicle = self.chronicle.as_mut().unwrap();

        let interval = chronicle::interval::Interval::new(
            chronicle.st.new_timepoint(),
            chronicle.st.new_timepoint(),
        );

        let start = interval.get_start();
        let end = interval.get_end();

        let result = chronicle.st.new_result();

        let name: Vec<VarId> = task
            .drain(..)
            .map(|cst| chronicle.st.new_cst(cst))
            .collect();

        let n_subtask = chronicle.get_subtasks().len();
        let label = Label::Action(n_subtask);

        let subtask = SubTask {
            interval,
            name: name.clone(),
            result,
            label: Some(label),
        };

        let binding = ActionBinding {
            name: name.clone(),
            index: chronicle.get_subtasks().len(),
            interval,
        };

        chronicle.add_subtask(subtask);
        chronicle.bindings.add_binding(label, binding);

        TaskRef { start, end, name }
    }
}
