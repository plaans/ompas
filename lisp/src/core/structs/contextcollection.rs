use crate::core::structs::lerror;
use crate::core::structs::lerror::LError;
use anyhow::anyhow;
use im::HashMap;
use std::any::Any;
use std::sync::Arc;

/// Struct Wrapping contexts (modules) for each library.
#[derive(Default, Clone, Debug)]
pub struct ContextCollection {
    inner: HashMap<String, Arc<dyn Any + Send + Sync>>,
    //map_label_usize: HashMap<String, usize>,
    //reverse_map: HashMap<usize, String>,
}

impl ContextCollection {
    ///Insert a new context
    pub fn insert(&mut self, ctx: Arc<dyn Any + Send + Sync>, label: String) {
        self.inner.insert(label, ctx);
    }
}

impl ContextCollection {
    pub fn get<T: Any + Send + Sync>(&self, label: &str) -> Result<&T, anyhow::Error> {
        let value = self
            .inner
            .get(label)
            .ok_or_else(|| LError::from(anyhow!("Context \"{}\" does not exist!", label)))?;

        let ctx: &T = value
            .downcast_ref::<T>()
            .ok_or_else(|| anyhow!("Impossible to downcast context"))?;

        Ok(ctx)
    }

    pub fn get_mut<T: Any + Send + Sync>(&mut self, label: &str) -> lerror::Result<&mut T> {
        let value = self
            .inner
            .get_mut(label)
            .ok_or_else(|| LError::from(anyhow!("Context \"{}\" does not exist!", label)))?;

        let any = Arc::get_mut(value)
            .ok_or_else(|| LError::from(anyhow!("Impossible to get mutable context")))?;

        let ctx: &mut T = any
            .downcast_mut::<T>()
            .ok_or_else(|| LError::from(anyhow!("Impossible to downcast context")))?;

        Ok(ctx)
    }
}
