use std::fmt::Display;

#[derive(Default, Clone, Debug)]
pub struct PureFonctionCollection {
    inner: im::HashSet<String>,
}

impl From<Vec<&'static str>> for PureFonctionCollection {
    fn from(vec: Vec<&'static str>) -> Self {
        let mut set = im::HashSet::default();
        for e in vec {
            set.insert(e.to_string());
        }
        Self { inner: set }
    }
}

impl PureFonctionCollection {
    pub fn append(&mut self, other: Self) {
        let new = self.inner.clone().union(other.inner);
        self.inner = new;
    }
    pub fn is_pure(&self, f: &str) -> bool {
        self.inner.contains(f)
    }
    pub fn add(&mut self, f: impl Display) {
        self.inner.insert(f.to_string());
    }
}
