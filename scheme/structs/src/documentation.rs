use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};

#[derive(Default, Debug, Clone)]
pub struct Documentation {
    inner: BTreeMap<String, LHelp>,
}

impl Documentation {
    pub fn append(&mut self, mut other: Self) {
        self.inner.append(&mut other.inner)
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
}
#[derive(Clone)]
pub struct LHelp {
    label: &'static str,
    short: &'static str,
    verbose: Option<&'static str>,
}

impl LHelp {
    pub fn new(label: &'static str, short: &'static str) -> LHelp {
        LHelp {
            label,
            short,
            verbose: None,
        }
    }

    pub fn new_verbose(label: &'static str, short: &'static str, verbose: &'static str) -> LHelp {
        LHelp {
            label,
            short,
            verbose: Some(verbose),
        }
    }
}

impl Debug for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}\n{}", self.short, self.verbose.unwrap_or(""))
    }
}

impl Display for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:<20}: {}", self.label, self.short)
    }
}

impl From<Vec<LHelp>> for Documentation {
    fn from(vec: Vec<LHelp>) -> Self {
        let mut result: BTreeMap<String, LHelp> = Default::default();
        for e in vec {
            result.insert(e.label.to_string(), e);
        }
        Self { inner: result }
    }
}
