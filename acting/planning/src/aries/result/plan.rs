use crate::aries::result::PlanResult;
use aries_model::extensions::AssignmentExt;
use aries_model::lang::Atom;
use aries_planning::chronicles::{ChronicleKind, ChronicleOrigin};
use im::HashMap;
use ompas_structs::planning::plan::{AbstractTaskInstance, ActionInstance, Plan, TaskInstance};
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;

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
