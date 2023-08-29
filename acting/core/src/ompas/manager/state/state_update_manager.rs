use crate::TOKIO_CHANNEL_SIZE;
use ompas_utils::other::get_and_update_id_counter;
use sompas_structs::lvalues::LValueS;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::mpsc;

/// List of facts that have been updated
pub type Updated = Vec<LValueS>;

pub enum StateRule {
    All,
    Specific(Vec<LValueS>),
}

struct StateUpdateSubscriberInterface {
    channel: mpsc::Sender<bool>,
    rule: StateRule,
}

pub struct StateUpdateSubscriber {
    pub channel: mpsc::Receiver<bool>,
    pub id: SubscriberId,
}

pub type SubscriberId = usize;

#[derive(Default)]
pub struct StateUpdateManager {
    inner: HashMap<SubscriberId, StateUpdateSubscriberInterface>,
    next_id: Arc<AtomicUsize>,
}

impl StateUpdateManager {
    pub async fn check_updates_and_send_notifications(&self, mut updated: Updated) {
        for subscriber in self.inner.values() {
            match &subscriber.rule {
                StateRule::All => {
                    let _ = subscriber.channel.send(true).await;
                }
                StateRule::Specific(rules) => {
                    let updated: Vec<String> = updated.drain(..).map(|u| u.to_string()).collect();
                    'rule: for rule in rules {
                        let rule_str = rule.to_string();
                        for update in &updated {
                            if update.contains(&rule_str) {
                                let _ = subscriber.channel.send(true).await;
                                break 'rule;
                            }
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
