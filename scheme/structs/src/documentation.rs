use std::collections::BTreeMap;
use std::fmt::Write;
use std::fmt::{Debug, Display, Formatter};
#[derive(Default, Debug, Clone)]
pub struct Documentation {
    inner: BTreeMap<String, LHelp>,
}

impl Documentation {
    /// Creates a new documentation for the module
    /// Automatically generates the verbose part of the module with the contained element of the module.
    pub fn new(mut mod_doc: LHelp, other: Vec<LHelp>) -> Self {
        let mut result: BTreeMap<String, LHelp> = Default::default();
        let mut verbose_mod: String = format!("Elements contained in {}:\n", mod_doc.label);
        for e in other {
            writeln!(verbose_mod, "- {}", e.label).unwrap();
            result.insert(e.label.to_string(), e);
        }
        mod_doc.verbose = Some(verbose_mod);
        result.insert(mod_doc.label.to_string(), mod_doc);
        Self { inner: result }
    }

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
    label: String,
    short: String,
    verbose: Option<String>,
}

impl LHelp {
    pub fn new(label: impl Display, short: impl Display) -> LHelp {
        LHelp {
            label: label.to_string(),
            short: short.to_string(),
            verbose: None,
        }
    }

    pub fn new_verbose(label: impl Display, short: impl Display, verbose: impl Display) -> LHelp {
        LHelp {
            label: label.to_string(),
            short: short.to_string(),
            verbose: Some(verbose.to_string()),
        }
    }
}

impl Debug for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}\n{}",
            self.short,
            self.verbose.as_ref().unwrap_or(&String::new())
        )
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
