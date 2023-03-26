use crate::model::chronicle;
use crate::model::chronicle::acting_binding::ActionBinding;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::subtask::SubTask;
use crate::model::chronicle::{Chronicle, ChronicleKind, Instantiation};
use crate::model::process_ref::Label;
use crate::model::sym_domain::{cst, Domain};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use sompas_structs::lvalue::LValue;

pub(crate) const ROOT: &str = "ROOT";

#[derive(Clone)]
pub struct ActingModel {
    pub lv: LValue,
    pub lv_om: LValue,
    pub lv_expanded: LValue,
    pub instantiations: Vec<Instantiation>,
    pub chronicle: Option<Chronicle>,
}

impl ActingModel {
    pub fn get_instantiated_chronicle(&self) -> Option<Chronicle> {
        self.chronicle
            .as_ref()
            .map(|c| c.instantiate(self.instantiations.clone()))
    }
}

pub struct TaskRef {
    pub start: VarId,
    pub end: VarId,
    pub name: Vec<VarId>,
}

impl ActingModel {
    pub fn root(st: RefSymTable) -> Self {
        let chronicle = Chronicle::new(ROOT, ChronicleKind::Root, st.clone());
        st.set_domain(&st.get_domain_id(chronicle.get_result()), Domain::nil());
        st.set_domain(
            &st.get_domain_id(chronicle.get_presence()),
            Domain::d_true(),
        );

        Self {
            lv: LValue::Nil,
            lv_om: LValue::Nil,
            lv_expanded: LValue::Nil,
            instantiations: vec![],
            chronicle: Some(chronicle),
        }
    }

    pub fn add_subtask(&mut self, mut task: Vec<cst::Cst>) -> TaskRef {
        let chronicle = self.chronicle.as_mut().unwrap();

        let st = chronicle.st.clone();

        let interval = chronicle::interval::Interval::new(st.new_timepoint(), st.new_timepoint());

        let start = interval.get_start();
        let end = interval.get_end();

        let result = st.new_result();
        st.set_domain(&st.get_domain_id(&result), Domain::nil());

        let name: Vec<VarId> = task.drain(..).map(|cst| st.new_cst(cst)).collect();

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
            task_id: chronicle.get_subtasks().len(),
            interval,
        };

        chronicle.add_constraint(Constraint::leq(
            chronicle.interval.get_start(),
            interval.get_start(),
        ));
        chronicle.add_constraint(Constraint::leq(
            interval.get_end(),
            chronicle.interval.get_end(),
        ));

        chronicle.add_subtask(subtask);
        chronicle.bindings.add_binding(label, binding);

        TaskRef { start, end, name }
    }
}
