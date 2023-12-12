use crate::ompas::interface::continuous_planning_mode::ContinuousPlanningMode;
use crate::ompas::interface::select_mode::SelectMode;
use atomic_float::AtomicF64;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

pub const MAX_REACTIVITY: f64 = 3600.0;

#[derive(Clone)]
pub struct Deliberation {}
#[derive(Clone)]
pub struct Planner {}

#[derive(Clone)]
pub struct Reactivity<T> {
    inner: Arc<AtomicF64>,
    phantom_data: PhantomData<T>,
}

impl<T> Default for Reactivity<T> {
    fn default() -> Self {
        Self {
            inner: Arc::new(AtomicF64::new(MAX_REACTIVITY)),
            phantom_data: Default::default(),
        }
    }
}

impl<T> Reactivity<T> {
    pub fn get_reactivity(&self) -> f64 {
        self.inner.load(Ordering::Relaxed)
    }

    pub fn get_duration(&self) -> Duration {
        let reactivity = self.get_reactivity();
        if reactivity > MAX_REACTIVITY {
            Duration::from_secs_f64(MAX_REACTIVITY)
        } else {
            Duration::from_secs_f64(reactivity)
        }
    }
}

#[derive(Default, Clone)]
pub struct DeliberationManager {
    select_mode: Arc<RwLock<SelectMode>>,
    continuous_planning_mode: Arc<RwLock<ContinuousPlanningMode>>,
    pre_compute_models: Arc<AtomicBool>,
    pub(crate) planner_reactivity: Reactivity<Planner>,
    pub(crate) deliberation_reactivity: Reactivity<Deliberation>,
}

impl DeliberationManager {
    pub async fn get_select_mode(&self) -> SelectMode {
        self.select_mode.read().await.clone()
    }

    pub async fn set_select_mode(&self, mode: SelectMode) {
        *self.select_mode.write().await = mode
    }

    pub async fn get_continuous_planning_mode(&self) -> ContinuousPlanningMode {
        *self.continuous_planning_mode.read().await
    }

    pub async fn set_continuous_planning_mode(&self, mode: ContinuousPlanningMode) {
        *self.continuous_planning_mode.write().await = mode
    }

    pub async fn get_pre_compute_models(&self) -> bool {
        self.pre_compute_models.load(Ordering::Acquire)
    }

    pub async fn set_pre_compute_models(&self, pre_compute_models: bool) {
        let prev = self.pre_compute_models.load(Ordering::Acquire);
        self.pre_compute_models
            .compare_exchange(
                prev,
                pre_compute_models,
                Ordering::Acquire,
                Ordering::Relaxed,
            )
            .unwrap();
    }

    pub fn get_planner_reactivity(&self) -> f64 {
        self.planner_reactivity.get_reactivity()
    }

    pub fn set_planner_reactivity(&self, planner_reactivity: f64) {
        let old = self.planner_reactivity.inner.load(Ordering::Acquire);
        self.planner_reactivity
            .inner
            .compare_exchange(
                old,
                planner_reactivity,
                Ordering::Acquire,
                Ordering::Relaxed,
            )
            .unwrap();
    }

    pub fn get_planner_reactivity_duration(&self) -> Duration {
        self.planner_reactivity.get_duration()
    }

    pub fn set_deliberation_reactivity(&self, deliberation_reactivity: f64) {
        let old = self.deliberation_reactivity.inner.load(Ordering::Acquire);
        self.deliberation_reactivity
            .inner
            .compare_exchange(
                old,
                deliberation_reactivity,
                Ordering::Acquire,
                Ordering::Relaxed,
            )
            .unwrap();
    }

    pub fn get_deliberation_reactivity(&self) -> f64 {
        self.deliberation_reactivity.get_reactivity()
    }

    pub fn get_deliberation_reactivity_duration(&self) -> Duration {
        self.deliberation_reactivity.get_duration()
    }
}
