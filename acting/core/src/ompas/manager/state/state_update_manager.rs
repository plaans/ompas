use crate::TOKIO_CHANNEL_SIZE;
use ompas_utils::other::get_and_update_id_counter;
use sompas_structs::lvalues::LValueS;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::mpsc;

/// List of facts that have been updated
pub type StateUpdate = Vec<LValueS>;

pub enum StateRule {
    All,
    Specific(Vec<LValueS>),
}

struct StateUpdateSubscriberInterface {
    channel: mpsc::Sender<StateUpdate>,
    rule: StateRule,
}

pub struct StateUpdateSubscriber {
    pub channel: mpsc::Receiver<StateUpdate>,
    pub id: SubscriberId,
}

pub type SubscriberId = usize;

#[derive(Default)]
pub struct StateUpdateManager {
    inner: HashMap<SubscriberId, StateUpdateSubscriberInterface>,
    next_id: Arc<AtomicUsize>,
}

impl StateUpdateManager {
    pub async fn check_updates_and_send_notifications(&self, updated: StateUpdate) {
        for subscriber in self.inner.values() {
            match &subscriber.rule {
                StateRule::All => {
                    let _ = subscriber.channel.send(updated.clone()).await;
                }
                StateRule::Specific(rules) => {
                    let string_updated: Vec<String> =
                        updated.iter().map(|u| u.to_string()).collect();
                    for rule in rules {
                        let rule_str = rule.to_string();
                        let mut concerned = vec![];
                        for (i, update) in string_updated.iter().enumerate() {
                            if update.contains(&rule_str) {
                                concerned.push(updated[i].clone());
                            }
                        }

                        if !concerned.is_empty() {
                            let _ = subscriber.channel.send(concerned).await;
                        }
                    }
                }
            };
        }
    }

    pub fn remove_subscriber(&mut self, subscriber_id: &SubscriberId) {
        self.inner.remove(subscriber_id);
    }

    pub fn new_subscriber(&mut self, rule: StateRule) -> StateUpdateSubscriber {
        let id = get_and_update_id_counter(self.next_id.clone());
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        self.inner
            .insert(id, StateUpdateSubscriberInterface { channel: tx, rule });

        StateUpdateSubscriber { channel: rx, id }
    }

    pub fn set_rule(&mut self, subscriber_id: &SubscriberId, rule: StateRule) {
        self.inner.get_mut(subscriber_id).unwrap().rule = rule
    }
}
