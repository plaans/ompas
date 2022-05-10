use crate::TaskId;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct TaskNetwork {
    parent: Arc<RwLock<Vec<TaskId>>>,
    pub subtasks: Arc<RwLock<im::HashMap<TaskId, Vec<TaskId>>>>,
}

impl TaskNetwork {
    /*
    FORMAT
     */
    fn format_network(tn: &im::HashMap<usize, Vec<usize>>, task_id: &TaskId) -> Vec<String> {
        let subtasks = tn.get(task_id).unwrap();
        let n = subtasks.len();
        if subtasks.is_empty() {
            return vec![format!("{:^3}", task_id), "   ".to_string()];
        } else {
            let mut vec = vec![];
            for (i, st) in subtasks.iter().enumerate() {
                for (j, line) in Self::format_network(tn, st).iter().enumerate() {
                    if j == 0 {
                        if i == 0 {
                            vec.push(format!("{:^3}---{}", task_id, line))
                        } else {
                            vec.push(format!(" |----{}", line))
                        }
                    } else if i < n - 1 {
                        vec.push(format!(" |    {}", line))
                    } else {
                        vec.push(format!("      {}", line))
                    }
                }
            }
            vec
        }
    }

    pub async fn format(&self) -> String {
        let tn = self.subtasks.read().await.clone();
        let parent: Vec<TaskId> = self.parent.read().await.clone();
        let mut str = String::new();
        str.push_str("Task Network:\n");
        for p in &parent {
            Self::format_network(&tn, p).iter().for_each(|s| {
                str.push_str(s.as_str());
                str.push('\n');
            });
            str.push('\n');
        }
        str
    }
    /*
    ADDERS
     */
    pub async fn add_task_to_parent(&self, parent_task: TaskId, task_id: TaskId) {
        self.subtasks
            .write()
            .await
            .get_mut(&parent_task)
            .unwrap()
            .push(task_id);
        self.subtasks.write().await.insert(task_id, vec![]);
    }
    pub async fn add_new_root_task(&self, task_id: TaskId) {
        self.subtasks.write().await.insert(task_id, vec![]);
        self.parent.write().await.push(task_id);
    }

    pub async fn get_number_of_subtasks(&self, id: &TaskId) -> TaskId {
        match self.subtasks.read().await.get(id) {
            Some(t) => t.len(),
            None => 0,
        }
    }

    pub async fn get_parents(&self) -> Vec<TaskId> {
        self.parent.read().await.clone()
    }

    pub async fn get_subtasks(&self, id: &TaskId) -> Vec<TaskId> {
        match self.subtasks.read().await.get(id) {
            Some(t) => t.to_vec(),
            None => vec![],
        }
    }
}
