use ompas_rae_structs::domain::RAEDomain;

pub struct CtxDomain {
    pub domain: RAEDomain,
}

impl CtxDomain {
    pub fn new(domain: RAEDomain) -> Self {
        Self { domain }
    }
}
