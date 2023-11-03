use crate::ompas::manager::acting::interval::Timepoint;
use crate::OMPAS_DELIBERATION_FREQUENCY;
use core::time::Duration;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use std::sync::Arc;
use tokio::sync::{watch, RwLock};
use tokio::time::Instant;

#[derive(Clone)]
pub struct ClockManager {
    instant: Arc<Instant>,
    clock_subscriber: Arc<RwLock<Option<watch::Receiver<u64>>>>,
}

impl Default for ClockManager {
    fn default() -> Self {
        Self {
            instant: Arc::new(Instant::now()),
            clock_subscriber: Arc::new(RwLock::new(None)),
        }
    }
}

impl ClockManager {
    pub fn now(&self) -> Timepoint {
        Timepoint::new_micros(self.instant.elapsed().as_micros())
    }

    /*pub async fn reset(&self) {
        *self.instant.write().await = Instant::now()
    }*/

    pub async fn start_global_clock(&self) {
        let period = 1_000_000 / OMPAS_DELIBERATION_FREQUENCY.get();
        let (tx, rx) = watch::channel(0);
        let mut process =
            ProcessInterface::new("GLOBAL_CLOCK", PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;
        tokio::spawn(
            async move {
                let mut tick = 0;
                'main: loop {
                    tokio::select! {
                        _ = process.recv() => {
                            break 'main;
                        }
                        _ = tokio::time::sleep(Duration::from_micros(period)) => {
                            //println!("{}", tick);
                            if tx.send(tick).is_err() {
                                //
                            }
                        }
                    }
                    tick += 1;
                }
            }, //}
        );

        *self.clock_subscriber.write().await = Some(rx);
    }

    pub async fn subscribe_to_clock(&self) -> watch::Receiver<u64> {
        let watch = self.clock_subscriber.read().await.clone();
        if let Some(watch) = watch {
            watch
        } else {
            self.start_global_clock().await;
            self.clock_subscriber.read().await.clone().unwrap()
        }
    }
}
