use crate::ompas::manager::acting::ActionId;
use crate::planning::planner::result::PlanResult;
use aries::model::extensions::AssignmentExt;
use aries::model::lang::Atom;
use aries_planning::chronicles::{ChronicleKind, ChronicleOrigin};
use im::HashMap;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::VecDeque;
use std::convert::TryFrom;

#[derive(Clone, Debug)]
pub struct Plan {
    pub chronicles: HashMap<ActionId, TaskInstance>,
}

impl Plan {
    pub fn get_root_task(&self) -> Option<ActionId> {
        let mut keys: Vec<usize> = self.chronicles.keys().cloned().collect();
        keys.sort_unstable();
        keys.first().cloned()
    }

    pub fn get_first_subtask(&self) -> Option<ActionId> {
        let mut keys: Vec<ActionId> = self.chronicles.keys().cloned().collect();
        keys.sort_unstable();
        keys.get(1).cloned()
    }

    pub fn extract_sub_plan(&self, task_id: ActionId) -> Plan {
        let mut subtasks: HashMap<usize, TaskInstance> = Default::default();

        let task = self.chronicles.get(&task_id).unwrap();
        subtasks.insert(task_id, task.clone());

        match task {
            TaskInstance::ActionInstance(_) => Plan {
                chronicles: subtasks,
            },
            TaskInstance::AbstractTaskInstance(a) => {
                let mut queue: VecDeque<ActionId> = a.subtasks.clone().into();
                while let Some(subtask) = queue.pop_front() {
                    let instance = self.chronicles.get(&subtask).unwrap();
                    if let TaskInstance::AbstractTaskInstance(a) = instance {
                        queue.append(&mut a.subtasks.clone().into());
                    }
                    subtasks.insert(subtask, instance.clone());
                }
                Plan {
                    chronicles: subtasks,
                }
            }
        }
    }

    /*
    FORMAT
     */
    fn format_abstract_task(&self, task: &AbstractTaskInstance, mut level: usize) -> String {
        let mut str = format!("{}*{} -> {}", "\t".repeat(level), task.task, task.method);
        level += 1;
        for t in &task.subtasks {
            str.push('\n');
            let subtask = &self.chronicles.get(t).unwrap();
            match subtask {
                TaskInstance::ActionInstance(a) => {
                    str.push_str(format!("{}*{}", "\t".repeat(level), a.inner).as_str())
                }
                TaskInstance::AbstractTaskInstance(a) => {
                    str.push_str(self.format_abstract_task(a, level).to_string().as_str())
                }
            }
        }
        str
    }

    pub fn format(&self) -> String {
        let mut str = "**Plan**\n".to_string();
        for (i, c) in &self.chronicles {
            match c {
                TaskInstance::ActionInstance(a) => {
                    str.push_str(format!("{:^3} : {}\n", i, a.inner).as_str());
                }
                TaskInstance::AbstractTaskInstance(a) => {
                    str.push_str(format!("{:^3} : {} -> {}", i, a.task, a.method).as_str());
                    for s in &a.subtasks {
                        str.push_str(format!(" {}", s).as_str())
                    }
                    str.push('\n');
                }
            }
        }
        str
    }

    pub fn format_hierarchy(&self) -> String {
        //println!("len: {}", self.chronicles.len());

        if self.chronicles.is_empty() {
            return "".to_string();
        }

        //let root = self.chronicles.get(&0).unwrap();

        let root_key = self.get_root_task().unwrap();
        let root = self.chronicles.get(&root_key).unwrap();
        match root {
            TaskInstance::ActionInstance(a) => {
                format!("{}", a.inner)
            }
            TaskInstance::AbstractTaskInstance(a) => self.format_abstract_task(a, 0),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TaskInstance {
    ActionInstance(ActionInstance),
    AbstractTaskInstance(AbstractTaskInstance),
}

impl From<ActionInstance> for TaskInstance {
    fn from(a: ActionInstance) -> Self {
        Self::ActionInstance(a)
    }
}

impl From<AbstractTaskInstance> for TaskInstance {
    fn from(a: AbstractTaskInstance) -> Self {
        Self::AbstractTaskInstance(a)
    }
}

impl TryFrom<TaskInstance> for ActionInstance {
    type Error = LRuntimeError;

    fn try_from(value: TaskInstance) -> Result<Self, Self::Error> {
        if let TaskInstance::ActionInstance(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}

impl TryFrom<TaskInstance> for AbstractTaskInstance {
    type Error = LRuntimeError;

    fn try_from(value: TaskInstance) -> Result<Self, Self::Error> {
        if let TaskInstance::AbstractTaskInstance(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}

#[derive(Clone, Debug)]
pub struct AbstractTaskInstance {
    pub task: LValue,
    pub method: LValue,
    pub subtasks: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct ActionInstance {
    pub inner: LValue,
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
