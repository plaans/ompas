pub mod monitor {
    use log::{info, warn};
    use ompas_lisp::core::eval;
    use ompas_lisp::core::structs::lenv::LEnv;
    use ompas_lisp::core::structs::lvalue::LValue;
    use ompas_utils::task_handler;
    use std::sync::Arc;
    use tokio::sync::{broadcast, mpsc, Mutex};

    lazy_static! {
        pub static ref MONITOR_COLLECTION: MonitorCollection = Default::default();
    }

    #[derive(Default, Clone)]
    pub struct MonitorCollection {
        inner: Arc<Mutex<Vec<Monitor>>>,
    }

    const TOKIO_CHANNEL_SIZE: usize = 64;

    pub async fn add_waiter(lambda: LValue) -> mpsc::Receiver<bool> {
        MONITOR_COLLECTION.add_waiter(lambda).await
    }

    impl MonitorCollection {
        async fn add_waiter(&self, lambda: LValue) -> mpsc::Receiver<bool> {
            let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
            let w = Monitor {
                lambda,
                channel: tx,
            };
            self.inner.lock().await.push(w);
            rx
        }
        pub async fn check_monitors(&self, mut env: LEnv) {
            let mut item_to_remove = vec![];
            let mut waiters = self.inner.lock().await;
            for (id, waiter) in waiters.iter().enumerate() {
                let result = eval(&waiter.lambda, &mut env).await;
                match result {
                    Ok(lv) => {
                        //info!("{} => {}", waiter.lambda, lv);
                        if let LValue::True = lv {
                            info!("Wait on {} is now true.", waiter.lambda);
                            waiter
                                .channel
                                .send(true)
                                .await
                                .expect("could not send true message to waiter");
                            item_to_remove.push(id);
                        } else {
                            //info!("{} is still false", waiter.lambda)
                        }
                    }
                    Err(e) => warn!("error checking wait on: {}", e),
                }
            }
            item_to_remove.iter().rev().for_each(|&i| {
                waiters.remove(i);
            })
        }
    }

    pub struct Monitor {
        lambda: LValue,
        channel: mpsc::Sender<bool>,
    }

    pub async fn get_debug() -> String {
        let lambdas: Vec<LValue> = MONITOR_COLLECTION
            .inner
            .lock()
            .await
            .iter()
            .map(|k| k.lambda.clone())
            .collect();
        let mut str = "'monitor' lambdas: \n".to_string();
        for l in lambdas {
            str.push('-');
            str.push_str(l.to_string().as_str());
            str.push('\n');
        }
        str
    }

    pub async fn task_check_monitor(mut receiver: broadcast::Receiver<bool>, env: LEnv) {
        //println!("task check wait on active");
        let mut end_receiver = task_handler::subscribe_new_task();
        loop {
            tokio::select! {
                _ = receiver.recv() => {
                    let n_wait_on = MONITOR_COLLECTION.inner.lock().await.len();
                    if n_wait_on != 0 {
                        MONITOR_COLLECTION.check_monitors(env.clone()).await;
                    }
                }
                _ = end_receiver.recv() => {
                    info!("Task \"task_check_monitor\" killed.");
                    break;
                }
            }
        }
        drop(receiver)
    }
}
