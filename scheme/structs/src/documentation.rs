use std::fmt::{Debug, Display, Formatter};
#[derive(Default, Debug, Clone)]
pub struct DocCollection {
    inner: im::OrdMap<String, Doc>,
}

impl Display for DocCollection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (label, doc) in &self.inner {
            writeln!(f, "~ {:<20}: {}", label, doc).unwrap();
        }
        Ok(())
    }
}

impl DocCollection {
    pub fn insert(&mut self, label: impl Display, doc: impl Into<Doc>) {
        self.inner.insert(label.to_string(), doc.into());
    }

    pub fn append(&mut self, other: Self) {
        for (k, v) in other.inner {
            self.inner.insert(k, v);
        }
    }

    pub fn get_all(&self) -> String {
        let mut string = String::new();
        for element in self.inner.iter() {
            string.push_str(format!("¤ {}\n", element.1).as_str())
        }
        string
    }

    pub fn get(&self, sym: &str) -> String {
        match self.inner.get(sym) {
            None => "no such function".to_string(),
            Some(h) => format!("{:?}\n", h),
        }
    }

    pub fn get_mut(&mut self, sym: &str) -> Option<&mut Doc> {
        self.inner.get_mut(sym)
    }
}
#[derive(Clone)]
pub struct Doc {
    //label: String,
    pub(crate) short: String,
    pub(crate) verbose: Option<String>,
}

impl Doc {
    pub fn new(short: impl Display) -> Doc {
        Doc {
            //label: label.to_string(),
            short: short.to_string(),
            verbose: None,
        }
    }

    pub fn new_verbose(short: impl Display, verbose: impl Display) -> Doc {
        Doc {
            //label: label.to_string(),
            short: short.to_string(),
            verbose: Some(verbose.to_string()),
        }
    }
}

impl From<&str> for Doc {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl From<String> for Doc {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

impl<T, U> From<(T, U)> for Doc
where
    T: Display,
    U: Display,
{
    fn from(doc: (T, U)) -> Self {
        Self::new_verbose(doc.0.to_string(), doc.1.to_string())
    }
}

impl Debug for Doc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}\n{}",
            self.short,
            self.verbose.as_ref().unwrap_or(&String::new())
        )
    }
}

impl Display for Doc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.short)
    }
}

/*impl From<Vec<Doc>> for DocCollection {
    fn from(vec: Vec<Doc>) -> Self {
        let mut result: BTreeMap<String, Doc> = Default::default();
        for e in vec {
            result.insert(e.label.to_string(), e);
        }
        Self { inner: result }
    }
}*/
