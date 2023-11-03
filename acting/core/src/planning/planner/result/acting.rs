use crate::model::chronicle::acting_process_model::{ActingProcessModel, ActingProcessModelLabel};
use crate::model::process_ref::ProcessRef;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::interval::{Interval, Timepoint};
use crate::ompas::manager::resource::WaiterPriority;
use crate::planning::planner::problem::ChronicleInstance;
use aries_planning::chronicles::TIME_SCALE;
use itertools::Itertools;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct RawArbitrary {
    lv: LValue,
}

pub struct RawSubTask {
    lv: LValue,
    interval: Interval,
}

pub struct RawAcquire {
    resource: String,
    quantity: usize,
    request: Timepoint,
    acquisition: Interval,
    priority: WaiterPriority,
}

pub struct RawRefinement {
    lv: LValue,
    interval: Interval,
}

pub enum RawChoice {
    Arbitrary(RawArbitrary),
    Acquire(RawAcquire),
    SubTask(RawSubTask),
    Refinement(RawRefinement),
}

impl Display for RawChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RawChoice::Arbitrary(a) => {
                write!(f, "{}", a.lv)
            }
            RawChoice::Acquire(a) => {
                write!(
                    f,
                    "[{},{},{}] acq({},{},{})",
                    a.request,
                    a.acquisition.start,
                    a.acquisition.end.unwrap(),
                    a.resource,
                    a.quantity,
                    a.priority
                )
            }
            RawChoice::SubTask(s) => {
                write!(f, "{} {}", s.interval, s.lv)
            }
            RawChoice::Refinement(r) => {
                write!(f, "{} {}", r.interval, r.lv)
            }
        }
    }
}

#[derive(Default)]
pub struct RawPlan {
    inner: HashMap<ProcessRef, RawChoice>,
}

impl Display for RawPlan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for e in &self.inner {
            writeln!(f, "{}:{}", e.0, e.1)?;
        }
        Ok(())
    }
}

pub fn extract_raw_plan(instances: &[ChronicleInstance]) -> RawPlan {
    let mut raw_plan = RawPlan::default();
    let mut resource_accesses: HashMap<String, Vec<(ProcessRef, RawAcquire)>> = Default::default();

    for instance in instances {
        let pr = instance.pr.clone();
        let chronicle = instance.am.chronicle.as_ref().unwrap();
        let st = &chronicle.st;

        let lv: Vec<LValue> = chronicle
            .get_name()
            .iter()
            .map(|var_id| st.get_domain_of_var(*var_id).as_cst().unwrap().into())
            .collect();
        let lv = lv.into();

        let interval = chronicle.interval;

        let start = st
            .get_domain_of_var(interval.get_start())
            .as_cst()
            .unwrap()
            .as_float()
            .unwrap();
        let start: Timepoint = Timepoint::new_with_factor(
            (start * TIME_SCALE.get() as f64) as u128,
            TIME_SCALE.get() as u64,
        );
        let end = st
            .get_domain_of_var(interval.get_end())
            .as_cst()
            .unwrap()
            .as_float()
            .unwrap();
        let end: Timepoint = Timepoint::new_with_factor(
            (end * TIME_SCALE.get() as f64) as u128,
            TIME_SCALE.get() as u64,
        );

        let interval = Interval::new(start, Some(end));

        raw_plan.inner.insert(
            pr.clone(),
            RawChoice::Refinement(RawRefinement { lv, interval }),
        );

        'choice: for (label, binding) in &chronicle.acting_process_models.inner {
            let mut pr = pr.clone();
            let ActingProcessModelLabel::Label(label) = label else {
                todo!()
            };
            pr.push(*label);
            let choice: RawChoice = match binding {
                ActingProcessModel::Arbitrary(a) => {
                    let lv: LValue = st
                        .get_domain_of_var(a.var_id)
                        .as_cst()
                        .unwrap()
                        .clone()
                        .into();
                    RawChoice::Arbitrary(RawArbitrary { lv })
                }
                ActingProcessModel::Action(s) => {
                    let lv: Vec<LValue> = s
                        .task
                        .name
                        .iter()
                        .map(|var_id| st.get_domain_of_var(*var_id).as_cst().unwrap().into())
                        .collect();
                    let lv = lv.into();

                    let start = st
                        .get_domain_of_var(s.task.interval.get_start())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let start: Timepoint = Timepoint::new_with_factor(
                        (start * TIME_SCALE.get() as f64) as u128,
                        TIME_SCALE.get() as u64,
                    );
                    let end = st
                        .get_domain_of_var(s.task.interval.get_end())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let end: Timepoint = Timepoint::new_with_factor(
                        (end * TIME_SCALE.get() as f64) as u128,
                        TIME_SCALE.get() as u64,
                    );

                    let interval = Interval::new(start, Some(end));

                    RawChoice::SubTask(RawSubTask { lv, interval })
                }
                ActingProcessModel::Resource(a) => {
                    let resource: String = a.resource.format(st, true);
                    let quantity: usize = st
                        .get_domain_of_var(a.quantity)
                        .as_cst()
                        .unwrap()
                        .as_int()
                        .unwrap() as usize;
                    let request = st
                        .get_domain_of_var(a.request)
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let request: Timepoint = Timepoint::new_with_factor(
                        (request * TIME_SCALE.get() as f64) as u128,
                        TIME_SCALE.get() as u64,
                    );

                    let start_acquisition = st
                        .get_domain_of_var(a.acquisition.get_start())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let start_acquisition: Timepoint = Timepoint::new_with_factor(
                        (start_acquisition * TIME_SCALE.get() as f64) as u128,
                        TIME_SCALE.get() as u64,
                    );

                    let end_acq = st
                        .get_domain_of_var(a.acquisition.get_end())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let end_acq: Timepoint = Timepoint::new_with_factor(
                        (end_acq * TIME_SCALE.get() as f64) as u128,
                        TIME_SCALE.get() as u64,
                    );
                    let acquisition = Interval::new(start_acquisition, Some(end_acq));

                    let raw_acquire = RawAcquire {
                        resource,
                        quantity,
                        request,
                        acquisition,
                        priority: WaiterPriority::Planner(0),
                    };

                    match resource_accesses.get_mut(&raw_acquire.resource) {
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

            raw_plan.inner.insert(pr, choice);
        }
    }

    for (_, mut accesses) in resource_accesses {
        accesses
            .drain(..)
            .sorted_by(|(_, a), (_, b)| {
                a.acquisition
                    .start
                    .as_secs()
                    .total_cmp(&b.acquisition.start.as_secs())
            })
            .enumerate()
            .for_each(|(id, (pr, mut ra))| {
                ra.priority = WaiterPriority::Planner(id);
                raw_plan.inner.insert(pr, RawChoice::Acquire(ra));
            });
    }

    raw_plan
}
