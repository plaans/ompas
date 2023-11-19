use crate::ompas::interface::rae_options::OMPASOptions;
use crate::ompas::interface::select_mode::SelectMode;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct OMPASManager {
    inner: Arc<RwLock<OMPASOptions>>,
}

impl OMPASManager {
    pub async fn set_pre_compute_models(&self, pre_compute_models: bool) {
        self.inner
            .write()
            .await
            .set_pre_compute_models(pre_compute_models)
    }

    pub async fn get_pre_compute_models(&self) -> bool {
        self.inner.read().await.get_pre_compute_models()
    }
    pub async fn get_select_mode(&self) -> SelectMode {
        *self.inner.read().await.get_select_mode()
    }

    pub async fn set_select_mode(&self, mode: SelectMode) {
        self.inner.write().await.set_select_mode(mode);
    }
}
