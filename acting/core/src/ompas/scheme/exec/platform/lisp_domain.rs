use sompas_structs::lmodule::InitScheme;
use std::fs;
use std::path::PathBuf;

#[derive(Clone)]
pub enum LispDomain {
    String(String),
    File(PathBuf),
}

impl From<LispDomain> for InitScheme {
    fn from(l: LispDomain) -> Self {
        match l {
            LispDomain::String(s) => vec![s].into(),
            LispDomain::File(f) => vec![fs::read_to_string(f).unwrap()].into(),
        }
    }
}

impl Default for LispDomain {
    fn default() -> Self {
        Self::String("()".to_string())
    }
}

impl From<&str> for LispDomain {
    fn from(s: &str) -> Self {
        Self::String(s.to_string())
    }
}

impl From<String> for LispDomain {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<PathBuf> for LispDomain {
    fn from(p: PathBuf) -> Self {
        Self::File(p)
    }
}
