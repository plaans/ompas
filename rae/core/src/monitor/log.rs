use ompas_middleware::{LogLevel, Master, LOG_TOPIC_ROOT};
use ompas_rae_interface::LOG_TOPIC_PLATFORM;
use ompas_rae_language::LOG_TOPIC_OMPAS;
use sompas_macros::async_scheme_fn;
use sompas_structs::lruntimeerror::LRuntimeError;

const LOG_ROOT: &str = "log-root";
const LOG_PLATFORM: &str = "log-platform";
const LOG_OMPAS: &str = "log-ompas";

#[async_scheme_fn]
pub async fn activate_log(logs: Vec<String>) {
    for log in logs {
        let topic = match log.as_str() {
            LOG_ROOT => LOG_TOPIC_ROOT,
            LOG_PLATFORM => LOG_TOPIC_PLATFORM,
            LOG_OMPAS => LOG_TOPIC_OMPAS,
            _ => continue,
        };
        Master::start_display_log_topic(topic).await;
    }
}

#[async_scheme_fn]
pub async fn deactivate_log(logs: Vec<String>) {
    for log in logs {
        let topic = match log.as_str() {
            LOG_ROOT => LOG_TOPIC_ROOT,
            LOG_PLATFORM => LOG_TOPIC_PLATFORM,
            LOG_OMPAS => LOG_TOPIC_OMPAS,
            _ => continue,
        };
        Master::stop_display_log_topic(topic).await;
    }
}

#[async_scheme_fn]
pub async fn set_log_level(level: String) -> Result<(), LRuntimeError> {
    match LogLevel::try_from(level.as_str()) {
        Ok(level) => {
            Master::set_log_level(level).await;
            Ok(())
        }
        Err(_) => Err(LRuntimeError::new(
            "set_log_level",
            format!("expected values {{error, warn, info, debug, trace}}, got: {level}"),
        )),
    }
}

#[async_scheme_fn]
pub async fn get_log_level() -> String {
    Master::get_log_level().await.to_string()
}
