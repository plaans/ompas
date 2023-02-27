use crate::aries::BindingAriesAtoms;
use aries_model::extensions::{AssignmentExt, SavedAssignment, Shaped};
use aries_model::lang::{Atom, Variable};
use aries_planning::chronicles::{printer, ChronicleKind, ChronicleOrigin, FiniteProblem};
use im::HashMap;
use ompas_structs::conversion::chronicle::{Chronicle, Instantiation};
use ompas_structs::planning::plan::{AbstractTaskInstance, ActionInstance, Plan, TaskInstance};
use ompas_structs::planning::problem::PlanningProblem;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub struct PlanResult {
    pub ass: Arc<SavedAssignment>,
    pub fp: FiniteProblem,
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
        printer::Printer::print_chronicle(&chronicle, model)
    }
}

pub fn instantiate_chronicles(
    pp: &PlanningProblem,
    pr: &PlanResult,
    bindings: &BindingAriesAtoms,
) -> Vec<Chronicle> {
    let mut instances = vec![];
    let ass = &pr.ass;
    let model = &pr.fp.model;
    let st = pp.st.clone();
    for chronicle in &pp.instance.instances {
        let mut instantiations = vec![];

        for var in &chronicle.chronicle.variables {
            let value = match bindings.get_var(var).unwrap() {
                Variable::Bool(b) => st.new_bool(ass.boolean_value_of(*b).unwrap()),
                Variable::Int(i) => {
                    let value = ass.var_domain(*i).lb;
                    st.new_int(value as i64)
                }
                Variable::Fixed(f) => {
                    let value = ass.f_domain(*f).lb();
                    st.new_float(value as f64)
                }
                Variable::Sym(s) => {
                    let sym = ass.sym_domain_of(*s).into_singleton().unwrap();
                    let value = model.get_symbol(sym);
                    st.new_symbol(value)
                }
            };
            instantiations.push(Instantiation::new(*var, value));
        }

        instances.push(chronicle.chronicle.instantiate(instantiations))
    }

    instances
}

pub fn extract_plan(pr: &PlanResult) -> Plan {
    let ass = &pr.ass;
    let problem = &pr.fp;

    /*let fmt1 = |x: &SAtom| -> LValue {
        let sym = ass.sym_domain_of(*x).into_singleton().unwrap();
        problem.model.shape.symbols.symbol(sym).to_string().into()
    };*/
    let fmt = |name: &[Atom]| -> LValue {
        let syms: Vec<LValue> = name
            .iter()
            .map(
                |x| match x {
                    Atom::Bool(b) => ass.value_of_literal(*b).unwrap().into(),
                    Atom::Int(i) => ass.var_domain(*i).lb.into(),
                    Atom::Fixed(f) => {
                        let float: f64 = ass.f_domain(*f).to_string().parse().unwrap();
                        float.into()
                    }
                    Atom::Sym(s) => problem
                        .model
                        .shape
                        .symbols
                        .symbol(ass.sym_domain_of(*s).into_singleton().unwrap())
                        .to_string()
                        .into(),
                }, //ass.sym_domain_of(*x).into_singleton().unwrap()
            )
            .collect();
        syms.into()
    };

    let chronicles: Vec<_> = problem
        .chronicles
        .iter()
        .enumerate()
        .filter(|ch| ass.boolean_value_of(ch.1.chronicle.presence) == Some(true))
        .collect();
    // sort by start times
    //chronicles.sort_by_key(|ch| ass.f_domain(ch.1.chronicle.start).num.lb);

    let get_subtasks_ids = |chronicle_id: usize| -> Vec<usize> {
        let mut vec = vec![];
        for &(i, ch) in &chronicles {
            match ch.origin {
                ChronicleOrigin::Refinement { instance_id, .. } if instance_id == chronicle_id => {
                    vec.push(i);
                }
                _ => (),
            }
        }
        vec
    };

    let mut map: HashMap<usize, TaskInstance> = Default::default();

    for &(i, ch) in &chronicles {
        match ch.chronicle.kind {
            ChronicleKind::Problem => {
                let instance = AbstractTaskInstance {
                    task: "root".into(),
                    method: "root".into(),
                    subtasks: get_subtasks_ids(i),
                };
                map.insert(i, instance.into());
            }
            ChronicleKind::Method => {
                let instance = AbstractTaskInstance {
                    task: fmt(ch.chronicle.task.as_ref().unwrap()),
                    method: fmt(&ch.chronicle.name),
                    subtasks: get_subtasks_ids(i),
                };
                map.insert(i, instance.into());
            }
            ChronicleKind::Action | ChronicleKind::DurativeAction => {
                let instance = ActionInstance {
                    inner: fmt(&ch.chronicle.name),
                };
                map.insert(i, instance.into());
            }
        }
    }

    Plan { chronicles: map }
}

pub fn extract_instantiated_methods(pr: &PlanResult) -> LResult {
    let ass = &pr.ass;
    let problem = &pr.fp;

    let methods: Vec<_> = pr
        .fp
        .chronicles
        .iter()
        .filter(|ch| {
            ass.boolean_value_of(ch.chronicle.presence) == Some(true)
                && ch.chronicle.kind == ChronicleKind::Method
        })
        .collect();

    let fmt1 = |x: &Atom| -> LValue {
        match x {
            Atom::Bool(b) => ass.value_of_literal(*b).unwrap().into(),
            Atom::Int(i) => ass.var_domain(*i).lb.into(),
            Atom::Fixed(f) => {
                let float: f64 = ass.f_domain(*f).to_string().parse().unwrap();
                float.into()
            }
            Atom::Sym(s) => problem
                .model
                .shape
                .symbols
                .symbol(ass.sym_domain_of(*s).into_singleton().unwrap())
                .to_string()
                .into(),
        }
    };
    let mut lv_methods: Vec<LValue> = vec![];
    for m in methods {
        let name: Vec<LValue> = m.chronicle.name.iter().map(|s| fmt1(s)).collect::<_>();
        lv_methods.push(name.into());
    }

    Ok(lv_methods.into())
}
