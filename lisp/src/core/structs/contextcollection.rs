use im::HashMap;
use std::any::Any;
use std::ops::Deref;
use std::sync::Arc;

/// Struct Wrapping contexts (modules) for each library.
#[derive(Default, Clone, Debug)]
pub struct ContextCollection {
    inner: Vec<Arc<dyn Any + Send + Sync>>,
    map_label_usize: HashMap<String, usize>,
    reverse_map: HashMap<usize, String>,
}

impl ContextCollection {
    ///Insert a new context
    pub fn insert(&mut self, ctx: Arc<dyn Any + Send + Sync>, label: String) -> usize {
        self.inner.push(ctx);
        let id = self.inner.len() - 1;
        self.map_label_usize.insert(label.clone(), id);
        self.reverse_map.insert(id, label);
        id
    }

    /// Returns a reference to the context with the corresponding id
    pub fn get_context(&self, id: usize) -> &(dyn Any + Send + Sync) {
        match self.inner.get(id) {
            None => panic!("id {} corresponds to no ctx:\n {:?}", id, self),
            Some(some) => some.deref(),
        }
    }

    /// Returns the context corresponding to the label.
    pub fn get_context_with_label(&self, label: &str) -> &dyn Any {
        let id = match self.map_label_usize.get(label) {
            None => panic!("no context with such label"),
            Some(s) => *s,
        };

        self.get_context(id)
    }

    /// Returns a mutable reference to the context with corresponding id
    pub fn get_mut_context(&mut self, id: usize) -> &mut (dyn Any + Send + Sync) {
        match self.inner.get_mut(id) {
            None => panic!("no context with id {}", 1),
            Some(ctx) => match Arc::get_mut(ctx) {
                None => panic!("Could no get mut ref from Arc of mod {}. This is probably because the reference to the context is shared", self.reverse_map.get(&id).unwrap()),
                Some(ctx) => ctx
            }
        }
    }
}
