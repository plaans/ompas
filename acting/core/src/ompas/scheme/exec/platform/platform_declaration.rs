use crate::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use crate::ompas::scheme::exec::platform::PlatformDescriptor;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Clone)]
pub enum PlatformDeclaration {
    Exec(Arc<RwLock<dyn PlatformDescriptor>>),
    Simu(LispDomain),
}

impl PlatformDeclaration {
    pub fn new(p: impl Into<PlatformDeclaration>) -> Self {
        p.into()
    }
}

/*impl From<LispDomain> for PlatformDeclaration {
    fn from(d: LispDomain) -> Self {
        Self::Simu(d)
    }
}*/

impl From<&str> for PlatformDeclaration {
    fn from(s: &str) -> Self {
        Self::Simu(s.into())
    }
}

impl From<String> for PlatformDeclaration {
    fn from(s: String) -> Self {
        Self::Simu(s.into())
    }
}

impl From<PathBuf> for PlatformDeclaration {
    fn from(p: PathBuf) -> Self {
        Self::Simu(p.into())
    }
}

impl<T> From<T> for PlatformDeclaration
where
    T: PlatformDescriptor,
{
    fn from(t: T) -> Self {
        Self::Exec(Arc::new(RwLock::new(t)))
    }
}
