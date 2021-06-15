#[derive(Debug, Clone)]
pub struct Job {
    _type: JobType,
    core: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum JobType {
    Task,
    Event,
}
pub type JobId = usize;
