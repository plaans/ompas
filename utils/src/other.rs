use async_recursion::async_recursion;
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

pub type DynFut<T> = ::std::pin::Pin<Box<dyn Send + ::std::future::Future<Output = T>>>;
pub type AsyncFn<T, F> = fn(T) -> DynFut<F>;

#[async_recursion]
pub async fn generic_race<T: std::marker::Send, F: std::marker::Send>(
    mut vec: Vec<T>,
    f: AsyncFn<T, F>,
) -> F {
    let len = vec.len();
    match len {
        0 => unreachable!(),
        1 => f(vec.remove(0)).await,
        l => {
            let mid = l / 2;
            let vec_b = vec.split_off(mid);
            let branch_a = generic_race(vec, f);
            let branch_b = generic_race(vec_b, f);
            let r;
            tokio::select! {
                a = branch_a => {
                    r=a;
                },
                b = branch_b => {
                    r=b
                },
            }
            r
        }
    }
}
