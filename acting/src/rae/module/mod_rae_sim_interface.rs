use core::mem;
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::structs::{GetModule, Module};

pub struct CtxRaeSimInterface {
    pub sim_env: LEnv,
    pub sim_ctxs: ContextCollection,
}

impl GetModule for CtxRaeSimInterface {
    fn get_module(self) -> Module {
        todo!()
    }
}

impl CtxRaeSimInterface {
    pub fn new(sim_env: LEnv, sim_ctxs: ContextCollection) -> Self {
        Self { sim_env, sim_ctxs }
    }

    pub fn add_domain_sim(&mut self, mut domain_sim: LEnv) {
        let env = mem::take(&mut self.sim_env);
        domain_sim.set_outer(env);
        self.sim_env = domain_sim;
    }
}

impl Documentation for CtxRaeSimInterface {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

/*Functions
test_preconditions
test_effects
select
 */
