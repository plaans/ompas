use crate::model::process_ref::{MethodId, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::resource::WaiterPriority;
use crate::planning::planner::problem::ChronicleInstance;
use std::fmt::{Display, Formatter};

#[derive(Default)]
pub struct ActingTreeUpdate {
    pub(crate) acting_models: Vec<ChronicleInstance>,
    pub(crate) choices: Vec<Choice>,
}

impl ActingTreeUpdate {
    pub fn add_am(&mut self, ci: ChronicleInstance) {
        self.acting_models.push(ci);
    }

    pub fn add_choice(&mut self, choice: Choice) {
        self.choices.push(choice);
    }
}

#[derive(Debug)]
pub struct Choice {
    pub(crate) process_ref: ProcessRef,
    pub(crate) choice_inner: ChoiceInner,
}

impl Choice {
    pub fn new(pr: impl Into<ProcessRef>, choice_inner: impl Into<ChoiceInner>) -> Self {
        Self {
            process_ref: pr.into(),
            choice_inner: choice_inner.into(),
        }
    }
}

#[derive(Debug)]
pub enum ChoiceInner {
    Arbitrary(ChoiceArbitrary),
    Acquire(ChoiceAcquire),
    SubTask(ChoiceSubTask),
    Refinement(ChoiceRefinement),
}

impl From<ChoiceArbitrary> for ChoiceInner {
    fn from(value: ChoiceArbitrary) -> Self {
        Self::Arbitrary(value)
    }
}

impl From<ChoiceAcquire> for ChoiceInner {
    fn from(value: ChoiceAcquire) -> Self {
        Self::Acquire(value)
    }
}

impl From<ChoiceSubTask> for ChoiceInner {
    fn from(value: ChoiceSubTask) -> Self {
        Self::SubTask(value)
    }
}

impl From<ChoiceRefinement> for ChoiceInner {
    fn from(value: ChoiceRefinement) -> Self {
        Self::Refinement(value)
    }
}

impl Display for ChoiceInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ChoiceInner::Arbitrary(a) => {
                write!(f, "{}", a.val)
            }
            ChoiceInner::Acquire(a) => {
                write!(
                    f,
                    "[{},{},{}] acq({},{},{})",
                    a.request, a.s_acq, a.e_acq, a.resource, a.quantity, a.priority
                )
            }
            ChoiceInner::SubTask(s) => {
                write!(f, "[{},{}]", s.start, s.end)?;
                for arg in &s.name {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
            ChoiceInner::Refinement(r) => {
                write!(f, "[{},{}] {}", r.start, r.end, r.method_id)?;
                for arg in &r.name {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
        }
    }
}
#[derive(Debug)]
pub struct ChoiceArbitrary {
    pub val: Cst,
}

#[derive(Debug)]
pub struct ChoiceSubTask {
    pub name: Vec<Cst>,
    pub start: Cst,
    pub end: Cst,
}

#[derive(Debug)]
pub struct ChoiceAcquire {
    pub resource: Cst,
    pub quantity: Cst,
    pub request: Cst,
    pub s_acq: Cst,
    pub e_acq: Cst,
    pub priority: WaiterPriority,
}

#[derive(Debug)]
pub struct ChoiceRefinement {
    pub name: Vec<Cst>,
    pub start: Cst,
    pub end: Cst,
    pub method_id: MethodId,
}
