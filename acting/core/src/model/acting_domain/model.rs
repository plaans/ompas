use crate::model::chronicle;
use crate::model::chronicle::acting_process_model::ActionModel;
use crate::model::chronicle::condition::Condition;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::effect::{Effect, EffectOperation};
use crate::model::chronicle::subtask::SubTask;
use crate::model::chronicle::{Chronicle, ChronicleKind, Instantiation, RuntimeInfo};
use crate::model::process_ref::Label;
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_domain::{cst, Domain};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::interval::{Interval, Timepoint};
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub(crate) const ROOT: &str = "ROOT";

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ModelKind {
    PlantModel,
    SimModel,
    PlanModel,
    CostModel,
}

#[derive(Default, Debug, Clone)]
pub struct ModelCollection {
    inner: HashMap<ModelKind, LValue>,
}

impl ModelCollection {
    pub fn insert(&mut self, model: LValue, kind: ModelKind) {
        self.inner.insert(kind, model);
    }

    pub fn get(&self, kind: &ModelKind) -> Option<LValue> {
        let r = self.inner.get(kind).cloned();
        if r.is_none() {
            match kind {
                ModelKind::PlantModel => self.get(&ModelKind::SimModel),
                ModelKind::SimModel => self.get(&ModelKind::PlanModel),
                ModelKind::PlanModel | ModelKind::CostModel => None,
            }
        } else {
            r
        }
    }
}

impl Display for ModelCollection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (kind, model) in &self.inner {
            writeln!(f, "{:?} : {}", kind, model)?
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct ActingModel {
    pub lv: LValue,
    pub lv_om: LValue,
    pub lv_expanded: Option<LValue>,
    pub runtime_info: RuntimeInfo,
    pub chronicle: Option<Chronicle>,
}

impl ActingModel {
    pub fn get_clean_instantiated_chronicle(&self) -> Option<Chronicle> {
        self.chronicle
            .as_ref()
            .map(|c| c.clone().instantiate_and_clean(self.runtime_info.clone()))
    }

    pub fn get_instantiated_chronicle(&self) -> Option<Chronicle> {
        self.chronicle.as_ref().map(|c| {
            c.clone()
                .add_models(vec![])
                .instantiate(self.runtime_info.instantiations().clone())
        })
    }
}
#[derive(Clone)]

pub struct TaskRef {
    pub start: VarId,
    pub end: VarId,
    pub name: Vec<VarId>,
}

#[derive(Clone)]
pub struct NewTask {
    pub start: Option<Timepoint>,
    pub args: Vec<cst::Cst>,
}

fn format_vec(f: &mut Formatter<'_>, vec: &[cst::Cst]) -> std::fmt::Result {
    for e in vec {
        write!(f, "{e} ")?;
    }
    Ok(())
}

impl Display for NewTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(start) = &self.start {
            write!(f, "{}", start)?;
        }
        format_vec(f, &self.args)?;

        Ok(())
    }
}
#[derive(Clone)]

pub struct Event {
    pub interval: Interval,
    pub sv: Vec<cst::Cst>,
    pub value: cst::Cst,
}

impl Display for Event {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.interval)?;

        format_vec(f, &self.sv)?;
        write!(f, "<- {}", self.value)
    }
}

#[derive(Clone)]
pub struct Goal {
    pub interval: Option<Interval>,
    pub sv: Vec<cst::Cst>,
    pub value: cst::Cst,
}

impl Display for Goal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(interval) = &self.interval {
            write!(f, "{}", interval)?;
        }
        format_vec(f, &self.sv)?;
        write!(f, "<- {}", self.value)
    }
}

impl ActingModel {
    pub fn root(st: &RefSymTable) -> Self {
        let chronicle = Chronicle::new(ROOT, ChronicleKind::Root, st.clone());
        st.set_domain(st.get_domain_id(chronicle.get_result()), Domain::nil());
        st.set_domain(
            st.get_domain_id(chronicle.get_presence()),
            Domain::d_true(),
        );

        let mut runtime_info: RuntimeInfo = Default::default();
        runtime_info.add_instantiation(Instantiation::new(
            chronicle.get_interval().get_start(),
            st.new_cst(Cst::Float(0.0)),
        ));

        Self {
            lv: LValue::Nil,
            lv_om: LValue::Nil,
            lv_expanded: Some(LValue::Nil),
            runtime_info,
            chronicle: Some(chronicle),
        }
    }

    pub fn add_event(&mut self, mut event: Event) {
        let chronicle = self.chronicle.as_mut().unwrap();
        let st = chronicle.st.clone();

        let mut interval = chronicle::interval::Interval::new_instantaneous(
            st.new_float(event.interval.start.as_secs()),
        );
        if let Some(end) = event.interval.end {
            interval.set_end(st.new_float(end.as_secs()))
        }

        let effect = Effect {
            interval,
            sv: event.sv.drain(..).map(|cst| st.new_cst(cst)).collect(),
            operation: EffectOperation::assign(st.new_cst(event.value)),
        };

        chronicle.add_constraint(Constraint::leq(
            chronicle.interval.get_start(),
            interval.get_start(),
        ));
        chronicle.add_constraint(Constraint::leq(
            interval.get_end(),
            chronicle.interval.get_end(),
        ));

        chronicle.add_effect(effect);
    }

    pub fn add_goal(&mut self, mut goal: Goal) {
        let chronicle = self.chronicle.as_mut().unwrap();
        let st = chronicle.st.clone();

        let interval = match goal.interval {
            Some(t) => {
                let mut interval = chronicle::interval::Interval::new_instantaneous(
                    st.new_float(t.start.as_secs()),
                );
                if let Some(end) = t.end {
                    interval.set_end(st.new_float(end.as_secs()))
                }
                interval
            }
            None => chronicle::interval::Interval::new_instantaneous(st.new_timepoint()),
        };

        let condition = Condition {
            interval,
            sv: goal.sv.drain(..).map(|cst| st.new_cst(cst)).collect(),
            value: st.new_cst(goal.value),
        };

        chronicle.add_constraint(Constraint::leq(
            chronicle.interval.get_start(),
            interval.get_start(),
        ));
        chronicle.add_constraint(Constraint::leq(
            interval.get_end(),
            chronicle.interval.get_end(),
        ));

        chronicle.add_condition(condition)
    }

    pub fn add_new_task(&mut self, mut task: NewTask, label: Label) -> TaskRef {
        let chronicle = self.chronicle.as_mut().unwrap();

        let st = chronicle.st.clone();

        let start = st.new_timepoint();
        if let Some(t_s) = task.start {
            let r = st.set_domain(st.get_domain_id(start), t_s.as_secs());
            assert!(r.is_none())
        }

        let interval = chronicle::interval::Interval::new(start, st.new_timepoint());

        let start = interval.get_start();
        let end = interval.get_end();

        let result = st.new_result();
        st.set_domain(st.get_domain_id(result), Domain::nil());

        let name: Vec<VarId> = task.args.drain(..).map(|cst| st.new_cst(cst)).collect();

        let subtask = SubTask {
            interval,
            name: name.clone(),
            result,
            label: Some(label),
        };

        let constraints = vec![
            Constraint::leq(chronicle.interval.get_start(), start),
            Constraint::leq(end, chronicle.interval.get_end()),
        ];

        let binding = ActionModel::new(subtask, constraints);

        chronicle.acting_process_models.add_binding(label, binding);

        TaskRef { start, end, name }
    }
}
