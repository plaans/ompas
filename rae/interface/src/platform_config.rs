use std::any::Any;
use std::fmt::Display;
use std::sync::Arc;

pub trait ConfigTrait: Any + Display + Send + Sync {}

impl<T> ConfigTrait for T where T: Any + Display + Send + Sync {}

#[derive(Clone)]
pub enum PlatformConfig {
    String(String),
    Any(Arc<dyn ConfigTrait>),
    None,
}

impl Default for PlatformConfig {
    fn default() -> Self {
        PlatformConfig::None
    }
}

pub enum InnerPlatformConfig<'a, T> {
    String(&'a str),
    Any(&'a T),
    None,
}

impl PlatformConfig {
    pub fn new_string(s: String) -> Self {
        Self::String(s)
    }

    pub fn new_none() -> Self {
        Self::None
    }

    pub fn new_any<T: ConfigTrait>(config: T) -> Self {
        Self::Any(Arc::new(config))
    }

    pub fn get_inner<T: ConfigTrait>(&self) -> InnerPlatformConfig<T> {
        match &self {
            Self::None => InnerPlatformConfig::None,
            Self::String(s) => InnerPlatformConfig::String(s.as_str()),
            Self::Any(any) => match <dyn Any>::downcast_ref::<T>(any) {
                Some(t) => InnerPlatformConfig::Any(t),
                None => InnerPlatformConfig::None,
            },
        }
    }
    pub fn format(&self) -> String {
        match &self {
            Self::None => "none".to_string(),
            Self::String(s) => s.to_string(),
            Self::Any(any) => any.to_string(),
        }
    }
}
