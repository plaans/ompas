use im::HashSet;

#[derive(Default, Clone)]
pub struct PureFonctionCollection {
    inner: HashSet<&'static str>,
}

impl From<Vec<&'static str>> for PureFonctionCollection {
    fn from(vec: Vec<&'static str>) -> Self {
        let mut set = HashSet::default();
        for e in vec {
            set.insert(e);
        }
        Self { inner: set }
    }
}

impl PureFonctionCollection {
    pub fn append(&mut self, other: Self) {
        self.inner = self.inner.union(&other.inner).cloned().collect();
    }
}

pub trait PureFonction {
    fn get_pure_fonctions_symbols(&self) -> PureFonctionCollection;
}

impl PureFonctionCollection {
    pub fn is_pure(&self, f: &str) -> bool {
        self.inner.contains(f)
    }
}
