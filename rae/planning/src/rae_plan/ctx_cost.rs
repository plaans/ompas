use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

pub const CTX_COST: &str = "CtxCost";

#[derive(Default, Clone)]
pub struct CtxCost {
    cost: Arc<AtomicU64>,
}

impl CtxCost {
    pub fn increase_cost(&self, cost: u64) {
        loop {
            let o_cost = self.cost.load(Ordering::Relaxed);
            if self
                .cost
                .compare_exchange(o_cost, o_cost + cost, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                break;
            };
        }
    }

    pub fn get_cost(&self) -> u64 {
        self.cost.load(Ordering::Relaxed)
    }
}
