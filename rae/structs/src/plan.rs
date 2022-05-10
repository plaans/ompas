use crate::TaskId;
use im::HashMap;
use sompas_structs::lerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::VecDeque;
use std::convert::TryFrom;

#[derive(Clone, Debug)]
pub struct Plan {
    pub chronicles: HashMap<TaskId, TaskInstance>,
}

impl Plan {
    pub fn get_root_task(&self) -> Option<TaskId> {
        let mut keys: Vec<usize> = self.chronicles.keys().cloned().collect();
        keys.sort_unstable();
        keys.first().cloned()
    }

    pub fn get_first_subtask(&self) -> Option<TaskId> {
        let mut keys: Vec<TaskId> = self.chronicles.keys().cloned().collect();
        keys.sort_unstable();
        keys.get(1).cloned()
    }

    pub fn extract_sub_plan(&self, task_id: TaskId) -> Plan {
        let mut subtasks: im::HashMap<usize, TaskInstance> = Default::default();

        let task = self.chronicles.get(&task_id).unwrap();
        subtasks.insert(task_id, task.clone());

        match task {
            TaskInstance::ActionInstance(_) => Plan {
                chronicles: subtasks,
            },
            TaskInstance::AbstractTaskInstance(a) => {
                let mut queue: VecDeque<TaskId> = a.subtasks.clone().into();
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
}

impl Plan {
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

/*
impl ActionInstance {
    pub fn format(&self, level: usize) -> String {
        format!("{}*{}", "\t".repeat(level), self.inner)
    }
}

impl AbstractTaskInstance {
    pub fn format(&self, level: usize) -> String {
        let mut str = format!("{}*{} -> {}", "\t".repeat(level), self.task, self.method);
        for t in &self.subtasks {
            str.push('\n');
            str.push_str(
                match t {
                    TaskInstance::ActionInstance(a) => a.format(level + 1),
                    TaskInstance::AbstractTaskInstance(a) => a.format(level + 1),
                }
                .as_str(),
            );
        }
        str.push('\n');
        str
    }
}*/
