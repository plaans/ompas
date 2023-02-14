use crate::acting_domain::method::Method;
use crate::interval::Interval;

pub type ObjId = usize;

pub struct Supervisor {
    inner: Vec<ActingObj>,
}

impl Supervisor {
    pub fn get_id(&self, path: Path) -> Option<ObjId> {
        match path {
            Path::Id(id) => Some(id),
            Path::Relative(id, mut labels) => {
                let mut id = id;
                labels.reverse();
                while let Some(label) = labels.pop() {
                    let obj = &self.inner[id];
                    match label {
                        Label::Subtask(t) => {
                            if let ActingObj::Method(m) = obj {
                                id = m.subtasks[t];
                            } else {
                                return None;
                            }
                        }
                        Label::Arbitrary(a) => {
                            if let ActingObj::Method(m) = obj {
                                id = m.arbitraries[a]
                            } else {
                                return None;
                            }
                        }
                        Label::Command(c) => {
                            if let ActingObj::Method(m) = obj {
                                id = m.commands[c]
                            } else {
                                return None;
                            }
                        }
                        Label::Method(m) => {
                            if let ActingObj::Task(t) = obj {
                                id = t.method[m]
                            } else {
                                return None;
                            }
                        }
                    }
                }
                Some(id)
            }
        }
    }
}

pub enum ActingObj {
    Command(CommandObj),
    Task(TaskObj),
    Method(MethodObj),
    Arbitrary(ArbitraryObj),
    Acquire(AcquireObj),
}

pub struct CommandObj {
    id: ObjId,
    interval: Interval,
}

pub struct TaskObj {
    id: ObjId,
    interval: Interval,
    method: Vec<ObjId>,
    executed_method: Option<usize>,
}

pub struct MethodObj {
    id: ObjId,
    interval: Interval,
    arbitraries: Vec<ObjId>,
    commands: Vec<ObjId>,
    subtasks: Vec<ObjId>,
}

pub struct AcquireObj {
    id: ObjId,
}

pub struct ArbitraryObj {
    id: ObjId,
}

pub enum Path {
    Id(ObjId),
    Relative(ObjId, Vec<Label>),
}

pub enum Label {
    Subtask(usize),
    Arbitrary(usize),
    Command(usize),
    Method(usize),
}
