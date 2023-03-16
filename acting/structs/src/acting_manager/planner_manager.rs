use crate::acting_manager::process::plan_var::{AsCst, ExecutionVar, PlanVar};
use crate::acting_manager::process::process_ref::ProcessRef;
use crate::execution::resource::WaiterPriority;
use crate::planning::instance::ChronicleInstance;
use crate::planning::om_binding::ChronicleBinding;
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::FormatWithSymTable;
use crate::sym_table::VarId;
use aries::model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries::model::lang::{Atom, Variable};
use aries::model::Model;
use aries_planning::chronicles::{FiniteProblem, VarLabel};
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct PlannerManager {
    pub(crate) plan_vars: Vec<PlanVar>,
    pub(crate) bindings: RefBindingPlanner,
}

impl PlannerManager {
    pub fn format_execution_var<T: Display + Clone + AsCst>(
        &self,
        execution_var: &ExecutionVar<T>,
    ) -> String {
        if let Some(val) = &execution_var.val {
            val.as_cst().unwrap().to_string()
        } else {
            if let Some(var) = &execution_var.plan_var_id {
                self.plan_vars[*var].to_string()
            } else {
                "".to_string()
            }
        }
    }

    pub async fn clear(&mut self) {
        self.plan_vars.clear();
        self.bindings.inner.write().await.clear();
    }
}

#[derive(Default, Clone)]
pub struct RefBindingPlanner {
    pub inner: Arc<RwLock<BindingPlanner>>,
}

#[derive(Default)]
pub struct BindingPlanner {
    inner: im::HashMap<VarId, Variable>,
    reverse: im::HashMap<Variable, VarId>,
}

impl BindingPlanner {
    pub fn clear(&mut self) {
        self.inner.clear();
        self.reverse.clear();
    }

    pub fn add_binding(&mut self, id: &VarId, var: &Variable) {
        self.inner.insert(*id, *var);
        self.reverse.insert(*var, *id);
    }

    pub fn contains(&mut self, id: &VarId) -> bool {
        self.inner.contains_key(id)
    }

    pub fn get_var(&self, id: &VarId) -> Option<&Variable> {
        self.inner.get(id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&VarId> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for BindingPlanner {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, id) in &self.reverse {
            str.push_str(
                format!("{:?} <- {}\n", Atom::from(*var), id.format(st, sym_version)).as_str(),
            )
        }
        str
    }
}

pub struct ActingPlanResult {
    pub instances: Vec<ChronicleInstance>,
    pub bindings: RefBindingPlanner,
    pub assignements: Arc<SavedAssignment>,
    pub finite_problem: Arc<FiniteProblem>,
}

#[derive(Default)]
pub struct ActingPlan {
    pub(crate) inner: HashMap<ProcessRef, ActingChoice>,
}

pub struct ChoiceArbitrary {
    pub(crate) val: Cst,
}

pub struct ChoiceSubTask {
    pub name: Vec<Cst>,
    pub start: Cst,
    pub end: Cst,
}

pub struct ChoiceAcquire {
    pub resource: Cst,
    pub quantity: Cst,
    pub request: Cst,
    pub s_acq: Cst,
    pub e_acq: Cst,
    pub priority: WaiterPriority,
}

pub struct ChoiceRefinement {
    pub start: Cst,
    pub end: Cst,
}

pub enum ActingChoice {
    Arbitrary(ChoiceArbitrary),
    Acquire(ChoiceAcquire),
    SubTask(ChoiceSubTask),
    Refinement(ChoiceRefinement),
}

impl Display for ActingChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingChoice::Arbitrary(a) => {
                write!(f, "{}", a.val)
            }
            ActingChoice::Acquire(a) => {
                write!(
                    f,
                    "[{},{},{}] acq({},{},{})",
                    a.request, a.s_acq, a.e_acq, a.resource, a.quantity, a.priority
                )
            }
            ActingChoice::SubTask(s) => {
                write!(f, "[{},{}] {:?}", s.start, s.end, s.name)
            }
            ActingChoice::Refinement(r) => {
                write!(f, "[{},{}]", r.start, r.end)
            }
        }
    }
}

impl Display for ActingPlan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for e in &self.inner {
            writeln!(f, "{}:{}", e.0, e.1)?;
        }
        Ok(())
    }
}

pub async fn extract_raw_plan(pr: &ActingPlanResult) -> ActingPlan {
    let ass = &pr.assignements;
    let model = &pr.finite_problem.model;
    let bindings = &pr.bindings.inner.read().await;
    let mut acting_plan = ActingPlan::default();
    let mut resource_accesses: HashMap<String, Vec<(ProcessRef, ChoiceAcquire)>> =
        Default::default();

    let var_id_as_cst = |st: &RefSymTable, var_id: &VarId| match st.var_as_cst(var_id) {
        Some(cst) => cst,
        None => get_var_as_cst(bindings, ass, model, var_id),
    };

    for instance in &pr.instances {
        let pr = instance.pr.clone();
        let chronicle = instance.om.chronicle.as_ref().unwrap();
        let st = &chronicle.st;

        let start = var_id_as_cst(st, &chronicle.interval.get_start());

        let end = var_id_as_cst(st, &chronicle.interval.get_end());

        acting_plan.inner.insert(
            pr.clone(),
            ActingChoice::Refinement(ChoiceRefinement { start, end }),
        );

        'choice: for (label, binding) in &chronicle.bindings.inner {
            let mut pr = pr.clone();
            pr.push(*label);
            let choice: ActingChoice = match binding {
                ChronicleBinding::Arbitrary(a) => {
                    let val = var_id_as_cst(st, &a.var_id);

                    ActingChoice::Arbitrary(ChoiceArbitrary { val })
                }
                ChronicleBinding::Action(action) => {
                    let name: Vec<Cst> = action
                        .name
                        .iter()
                        .map(|var_id| var_id_as_cst(st, var_id))
                        .collect();

                    let start = var_id_as_cst(st, &action.interval.get_start());

                    let end = var_id_as_cst(st, &action.interval.get_end());

                    ActingChoice::SubTask(ChoiceSubTask { name, start, end })
                }
                ChronicleBinding::Acquire(a) => {
                    let resource = var_id_as_cst(st, &a.resource);
                    let quantity = var_id_as_cst(st, &a.quantity);
                    let request = var_id_as_cst(st, &a.request);
                    let s_acq = var_id_as_cst(st, &a.acquisition.get_start());
                    let e_acq = var_id_as_cst(st, &a.acquisition.get_end());

                    let raw_acquire = ChoiceAcquire {
                        resource,
                        quantity,
                        request,
                        s_acq,
                        e_acq,
                        priority: WaiterPriority::Planner(0),
                    };

                    match resource_accesses.get_mut(&raw_acquire.resource.to_string()) {
                        None => {
                            resource_accesses
                                .insert(raw_acquire.resource.to_string(), vec![(pr, raw_acquire)]);
                        }
                        Some(vec) => {
                            vec.push((pr, raw_acquire));
                        }
                    };
                    continue 'choice;
                }
            };

            acting_plan.inner.insert(pr, choice);
        }
    }

    for (_, mut accesses) in resource_accesses {
        accesses
            .drain(..)
            .sorted_by(|(_, a), (_, b)| {
                a.s_acq
                    .as_float()
                    .unwrap()
                    .total_cmp(&b.s_acq.as_float().unwrap())
            })
            .enumerate()
            .for_each(|(id, (pr, mut ra))| {
                ra.priority = WaiterPriority::Planner(id);
                acting_plan.inner.insert(pr, ActingChoice::Acquire(ra));
            });
    }

    acting_plan
}

pub fn get_var_as_cst(
    bindings: &BindingPlanner,
    ass: &Arc<SavedAssignment>,
    model: &Model<VarLabel>,
    var: &VarId,
) -> Cst {
    match bindings
        .get_var(var)
        .unwrap_or_else(|| panic!("{var} has no binding in the planner"))
    {
        Variable::Bool(b) => {
            let lit = b.true_lit();
            let value = ass.value(lit).unwrap();
            //println!("{:?}, {:?}, value: {}", b, lit, value);
            Cst::Bool(value)
        }
        Variable::Int(i) => Cst::Int(ass.var_domain(*i).lb as i64),
        Variable::Fixed(f) => Cst::Float(ass.f_domain(*f).lb() as f64),
        Variable::Sym(s) => {
            let sym = ass.sym_domain_of(*s).into_singleton().unwrap();
            let value = model.get_symbol(sym);
            Cst::Symbol(value.to_string())
        }
    }
}
