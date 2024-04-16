use sompas_structs::lmodule::InitScheme;
use std::fs;
use std::path::PathBuf;

#[derive(Clone)]
pub enum SchemeDomain {
    String(String),
    File(PathBuf),
}

impl From<SchemeDomain> for InitScheme {
    fn from(l: SchemeDomain) -> Self {
        match &l {
            SchemeDomain::String(s) => vec![s].into(),
            SchemeDomain::File(f) => vec![fs::read_to_string(f)
                .unwrap_or_else(|e| panic!("Error loading file {:?}:\n{}", f, e))]
            .into(),
        }
    }
}

impl Default for SchemeDomain {
    fn default() -> Self {
        Self::String("()".to_string())
    }
}

impl From<&str> for SchemeDomain {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
    }
}

impl From<String> for SchemeDomain {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<PathBuf> for SchemeDomain {
    fn from(p: PathBuf) -> Self {
        Self::File(p)
    }
}
