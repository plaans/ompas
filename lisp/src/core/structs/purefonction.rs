use im::HashSet;

#[derive(Default, Clone, Debug)]
pub struct PureFonctionCollection {
    inner: HashSet<String>,
}

impl From<Vec<&'static str>> for PureFonctionCollection {
    fn from(vec: Vec<&'static str>) -> Self {
        let mut set = HashSet::default();
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
}

pub trait PureFonction {
    fn get_pure_fonctions_symbols(&self) -> PureFonctionCollection;
}

impl PureFonctionCollection {
    pub fn is_pure(&self, f: &str) -> bool {
        self.inner.contains(f)
    }
}
