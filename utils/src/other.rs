use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

pub fn get_and_update_id_counter(aau: Arc<AtomicUsize>) -> usize {
    loop {
        let id = aau.load(Ordering::Relaxed);
        if aau
            .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed) //Equivalent to compare_and_swap
            .is_ok()
        {
            return id;
        }
    }
}
