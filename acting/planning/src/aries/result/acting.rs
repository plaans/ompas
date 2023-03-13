use crate::aries::OMPAS_TIME_SCALE;
use itertools::Itertools;
use ompas_structs::acting_manager::interval::{Interval, Timepoint};
use ompas_structs::acting_manager::process::plan_var::AsCst;
use ompas_structs::acting_manager::process::process_ref::ProcessRef;
use ompas_structs::execution::resource::WaiterPriority;
use ompas_structs::planning::instance::ChronicleInstance;
use ompas_structs::planning::om_binding::ChronicleBinding;
use ompas_structs::sym_table::r#trait::FormatWithSymTable;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct ActingPoint {
    pr: ProcessRef,
    choice: ActingChoice,
}

impl Display for ActingPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.pr, self.choice)
    }
}

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
        let chronicle = instance.om.chronicle.as_ref().unwrap();
        let st = &chronicle.st;

        let lv: Vec<LValue> = chronicle
            .get_name()
            .iter()
            .map(|var_id| {
                st.get_domain_of_var(var_id)
                    .as_cst()
                    .unwrap()
                    .clone()
                    .into()
            })
            .collect();
        let lv = lv.into();

        let interval = chronicle.interval;

        let start = st
            .get_domain_of_var(&interval.get_start())
            .as_cst()
            .unwrap()
            .as_float()
            .unwrap();
        let start: Timepoint = Timepoint::new_with_factor(
            (start * OMPAS_TIME_SCALE as f64) as u128,
            OMPAS_TIME_SCALE as u64,
        );
        let end = st
            .get_domain_of_var(&interval.get_end())
            .as_cst()
            .unwrap()
            .as_float()
            .unwrap();
        let end: Timepoint = Timepoint::new_with_factor(
            (end * OMPAS_TIME_SCALE as f64) as u128,
            OMPAS_TIME_SCALE as u64,
        );

        let interval = Interval::new(start, Some(end));

        raw_plan.inner.insert(
            pr.clone(),
            RawChoice::Refinement(RawRefinement { lv, interval }),
        );

        'choice: for (label, binding) in &chronicle.bindings.inner {
            let mut pr = pr.clone();
            pr.push(*label);
            let choice: RawChoice = match binding {
                ChronicleBinding::Arbitrary(a) => {
                    let lv: LValue = st
                        .get_domain_of_var(&a.var_id)
                        .as_cst()
                        .unwrap()
                        .clone()
                        .into();
                    RawChoice::Arbitrary(RawArbitrary { lv })
                }
                ChronicleBinding::Action(s) => {
                    let lv: Vec<LValue> = s
                        .name
                        .iter()
                        .map(|var_id| {
                            st.get_domain_of_var(var_id)
                                .as_cst()
                                .unwrap()
                                .clone()
                                .into()
                        })
                        .collect();
                    let lv = lv.into();

                    let start = st
                        .get_domain_of_var(&s.interval.get_start())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let start: Timepoint = Timepoint::new_with_factor(
                        (start * OMPAS_TIME_SCALE as f64) as u128,
                        OMPAS_TIME_SCALE as u64,
                    );
                    let end = st
                        .get_domain_of_var(&s.interval.get_end())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let end: Timepoint = Timepoint::new_with_factor(
                        (end * OMPAS_TIME_SCALE as f64) as u128,
                        OMPAS_TIME_SCALE as u64,
                    );

                    let interval = Interval::new(start, Some(end));

                    RawChoice::SubTask(RawSubTask { lv, interval })
                }
                ChronicleBinding::Acquire(a) => {
                    let resource: String = a.resource.format(st, true);
                    let quantity: usize = st
                        .get_domain_of_var(&a.quantity)
                        .as_cst()
                        .unwrap()
                        .as_int()
                        .unwrap() as usize;
                    let request = st
                        .get_domain_of_var(&a.request)
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let request: Timepoint = Timepoint::new_with_factor(
                        (request * OMPAS_TIME_SCALE as f64) as u128,
                        OMPAS_TIME_SCALE as u64,
                    );

                    let start_acquisition = st
                        .get_domain_of_var(&a.acquisition.get_start())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let start_acquisition: Timepoint = Timepoint::new_with_factor(
                        (start_acquisition * OMPAS_TIME_SCALE as f64) as u128,
                        OMPAS_TIME_SCALE as u64,
                    );

                    let end_acq = st
                        .get_domain_of_var(&a.acquisition.get_end())
                        .as_cst()
                        .unwrap()
                        .as_float()
                        .unwrap();
                    let end_acq: Timepoint = Timepoint::new_with_factor(
                        (end_acq * OMPAS_TIME_SCALE as f64) as u128,
                        OMPAS_TIME_SCALE as u64,
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

pub enum ActingChoice {
    Arbitrary(LValue),
    Acquire(String, WaiterPriority),
    SubTask(String),
    Refinement(LValue),
}

impl Display for ActingChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingChoice::Arbitrary(arb) => {
                write!(f, "{}", arb)
            }
            ActingChoice::Acquire(res, prio) => {
                write!(f, "{},{}", res, prio)
            }
            ActingChoice::SubTask(s) => {
                write!(f, "{}", s)
            }
            ActingChoice::Refinement(r) => {
                write!(f, "{}", r)
            }
        }
    }
}
