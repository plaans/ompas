use crate::ompas::manager::acting::interval::Timepoint;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::Instant;

#[derive(Clone)]
pub struct ClockManager {
    instant: Arc<RwLock<Instant>>,
}

impl Default for ClockManager {
    fn default() -> Self {
        Self {
            instant: Arc::new(RwLock::new(Instant::now())),
        }
    }
}

impl ClockManager {
    pub async fn now(&self) -> Timepoint {
        self.instant.read().await.elapsed().as_millis().into()
    }

    pub async fn reset(&self) {
        *self.instant.write().await = Instant::now()
    }
}
