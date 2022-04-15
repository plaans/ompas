use crate::lerror;
use crate::lerror::LError;
use anyhow::anyhow;
use im::HashMap;
use std::any::Any;
use std::sync::Arc;

pub type AsyncLTrait = dyn Any + Send + Sync;

#[derive(Clone, Debug)]
pub struct Context {
    inner: Arc<AsyncLTrait>,
}

impl Context {
    pub fn new<T: Any + Send + Sync>(ctx: T) -> Self {
        Self {
            inner: Arc::new(ctx),
        }
    }
}

/// Struct Wrapping contexts (modules) for each library.
#[derive(Default, Clone, Debug)]
pub struct ContextCollection {
    inner: HashMap<String, Context>,
    //map_label_usize: HashMap<String, usize>,
    //reverse_map: HashMap<usize, String>,
}

impl ContextCollection {
    ///Insert a new context
    pub fn insert(&mut self, ctx: Context, label: String) {
        self.inner.insert(label, ctx);
    }

    pub fn get_list_modules(&self) -> Vec<String> {
        self.inner.keys().cloned().collect()
    }
}

impl ContextCollection {
    pub fn get<T: Any + Send + Sync>(&self, label: &str) -> lerror::Result<&T> {
        let value = self.inner.get(label).ok_or_else(|| {
            LError::from(anyhow!(
                "In ContextCollection::get, context \"{}\" does not exist!",
                label
            ))
        })?;

        let ctx: &T = value
            .inner
            .downcast_ref::<T>()
            .ok_or_else(|| LError::from(anyhow!("Impossible to downcast context")))?;

        Ok(ctx)
    }
}
