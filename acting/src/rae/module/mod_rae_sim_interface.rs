use core::mem;
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::structs::LError::WrongNumberOfArgument;
use ompas_lisp::structs::LValue::True;
use ompas_lisp::structs::{GetModule, LError, LValue, Module};
use std::sync::Arc;

/*
LANGAUGE
 */

pub const CHECK_PRECONDITIONS: &str = "check-precondiitons";
pub const MOD_CTX_RAE_SIM_INTERFACE: &str = "rae-sim-interface";

pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable? \
    (lambda args\
        (check_preconditions args (rae-get-state))))";

pub struct CtxRaeSimInterface {
    pub sim_env: LEnv,
    pub sim_ctxs: ContextCollection,
}

impl GetModule for CtxRaeSimInterface {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_IS_APPLICABLE].into(),
            label: MOD_CTX_RAE_SIM_INTERFACE.to_string(),
        };
        module.add_fn_prelude(CHECK_PRECONDITIONS, check_preconditions);
        module
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

/// Example:
/// (check-preconditions (<method> <params>) state)
pub fn check_preconditions(
    args: &[LValue],
    _: &LEnv,
    _: &CtxRaeSimInterface,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        Err(WrongNumberOfArgument(
            CHECK_PRECONDITIONS,
            args.into(),
            args.len(),
            2..2,
        ))
    } else {
        let instantiated_method = &args[0];
        let state = &args[1];

        Ok(True)
    }
}

/*Functions
test_preconditions
test_effects
select
 */
