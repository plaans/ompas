use crate::domain::RAEDomain;

pub const CTX_DOMAIN: &str = "CtxDomain";
pub struct CtxDomain {
    pub domain: RAEDomain,
}

impl CtxDomain {
    pub fn new(domain: RAEDomain) -> Self {
        Self { domain }
    }
}
