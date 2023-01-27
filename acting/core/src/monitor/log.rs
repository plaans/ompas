use ompas_language::interface::LOG_TOPIC_PLATFORM;
use ompas_language::monitor::log::*;
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::{LogLevel, Master, LOG_TOPIC_ROOT};
use sompas_macros::async_scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;

#[derive(Default)]
pub struct ModLog {}

impl From<ModLog> for LModule {
    fn from(m: ModLog) -> Self {
        let mut module = LModule::new(m, MOD_LOG, DOC_MOD_LOG);
        module.add_async_fn(ACTIVATE_LOG, activate_log, DOC_ACTIVATE_LOG, false);
        module.add_async_fn(DEACTIVATE_LOG, deactivate_log, DOC_DEACTIVATE_LOG, false);
        module.add_async_fn(SET_LOG_LEVEL, set_log_level, DOC_SET_LOG_LEVEL, false);
        module.add_async_fn(GET_LOG_LEVEL, get_log_level, DOC_GET_LOG_LEVEL, false);

        module
    }
}

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
