use crate::lruntimeerror;
use crate::lruntimeerror::LRuntimeError;
use anyhow::anyhow;
use im::HashMap;
use std::any::Any;
use std::fmt::Display;
use std::sync::Arc;

pub type AsyncLTrait = dyn Any + Send + Sync;

#[derive(Clone, Debug)]
pub struct Context {
    label: String,
    inner: Arc<AsyncLTrait>,
}

impl Context {
    pub fn new<T: Any + Send + Sync>(ctx: T, label: impl Display) -> Self {
        Self {
            label: label.to_string(),
            inner: Arc::new(ctx),
        }
    }

    pub fn get_label(&self) -> &str {
        &self.label
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
    pub fn insert(&mut self, ctx: Context) {
        self.inner.insert(ctx.label.clone(), ctx);
    }

    pub fn contains(&self, label: impl Display) -> bool {
        self.inner.contains_key(&label.to_string())
    }

    pub fn get_contexts_labels(&self) -> Vec<String> {
        self.inner.keys().cloned().collect()
    }
}

impl ContextCollection {
    pub fn get<T: Any + Send + Sync>(&self, label: &str) -> lruntimeerror::Result<&T> {
        let value = self.inner.get(label).ok_or_else(|| {
            LRuntimeError::from(anyhow!(
                "In ContextCollection::get, context \"{}\" does not exist!",
                label
            ))
        })?;

        let ctx: &T = value
            .inner
            .downcast_ref::<T>()
            .ok_or_else(|| LRuntimeError::from(anyhow!("Impossible to downcast context")))?;

        Ok(ctx)
    }
}
