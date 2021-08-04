pub mod wait_on {
    use crate::rae::context::RAEEnv;
    use log::info;
    use ompas_lisp::core::{eval, ContextCollection, LEnv};
    use ompas_lisp::structs::LValue;
    use ompas_utils::task_handler;
    use std::sync::Arc;
    use std::thread;
    use tokio::sync::{broadcast, mpsc, Mutex};

    #[allow(deprecated)]
    lazy_static! {
        pub static ref WAIT_ON_COLLECTION: WaitOnCollection = Default::default();
    }

    #[derive(Default, Clone)]
    pub struct WaitOnCollection {
        inner: Arc<Mutex<Vec<WaitOn>>>,
    }

    const TOKIO_CHANNEL_SIZE: usize = 64;

    pub fn add_waiter(lambda: LValue) -> mpsc::Receiver<bool> {
        let handle = tokio::runtime::Handle::current();
        thread::spawn(move || {
            handle.block_on(async move { WAIT_ON_COLLECTION.add_waiter(lambda).await })
        })
        .join()
        .expect("")
    }

    impl WaitOnCollection {
        async fn add_waiter(&self, lambda: LValue) -> mpsc::Receiver<bool> {
            let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
            let w = WaitOn {
                lambda,
                channel: tx,
            };
            self.inner.lock().await.push(w);
            rx
        }
        pub async fn check_wait_on(&self, mut env: LEnv, mut ctxs: ContextCollection) {
            let mut item_to_remove = vec![];
            let mut waiters = self.inner.lock().await;
            for (id, waiter) in waiters.iter().enumerate() {
                if let LValue::True =
                    eval(&waiter.lambda, &mut env, &mut ctxs).unwrap_or(LValue::Nil)
                {
                    info!("Wait on {} is now true.", waiter.lambda);
                    waiter
                        .channel
                        .send(true)
                        .await
                        .expect("could not send true message to waiter");
                    item_to_remove.push(id);
                }
            }
            item_to_remove.iter().rev().for_each(|&i| {
                waiters.remove(i);
            })
        }
    }

    pub struct WaitOn {
        lambda: LValue,
        channel: mpsc::Sender<bool>,
    }

    pub async fn task_check_wait_on(
        mut receiver: broadcast::Receiver<bool>,
        env: LEnv,
        ctxs: ContextCollection,
    ) {
        println!("task check wait on active");
        let mut end_receiver = task_handler::subscribe_new_task();
        loop {
            tokio::select! {
                _ = receiver.recv() => {
                    let n_wait_on = WAIT_ON_COLLECTION.inner.lock().await.len();
                    if n_wait_on != 0 {
                        println!("{} wait ons to check!", n_wait_on);
                        WAIT_ON_COLLECTION.check_wait_on(env.clone(), ctxs.clone()).await;
                    }
                }
                _ = end_receiver.recv() => {
                    println!("Task \"task_check_wait_on\" killed.");
                    break;
                }
            }
        }
    }
}
