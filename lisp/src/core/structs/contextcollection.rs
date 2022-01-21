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

impl<T> ContextCollection {
    pub fn get(&self, label: &str) -> Result<&T, anyhow::Error> {
        let value = match self.inner.get(label) {
            Some(t) => t,
            None => return Err(anyhow!("Context \"{}\" does not exist!", label)),
        };
        let ctx: &T = value
            .downcast_ref::<T>()
            .ok_or_else(|| anyhow!("Impossible to downcast context"))?;

        Ok(ctx)
    }

    pub fn get_mut(&self, label: &str) -> Result<&mut T, anyhow::Error> {
        let value = match self.inner.get(label) {
            Some(t) => t,
            None => return Err(anyhow!("Context \"{}\" does not exist!", label)),
        };
        let ctx: &mut T = value
            .downcast_mut::<T>()
            .ok_or_else(|| anyhow!("Impossible to downcast context"))?;

        Ok(ctx)
    }
}
