//! [Deprecated]
//! Previous work to develop a dumb controller to test lisp integration

use ompas_lisp::core::structs::module::{GetModule, Module};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use std::sync::Arc;

//LANGUAGE

pub const MOD_DUMBER: &str = "mod-dumber";
pub const DOC_MOD_DUMBER: &str = "functions:\n\
                                \t-dumber_robot";

pub const LAMBDA_DUMBER_ROBOT: &str = "(define dumber_robot
    (lambda (r)
        (begin
            (if (and (> (robot.battery r) 0)
                    (= (robot.velocity r) (list 0 0)))
                (if (! (robot.in_station r))
                    (if (<= (robot.battery r) 0.4)
                        (robot.navigate_to r 7 17)
                        (robot.navigate_to r (rand-int-in-range 10 50) (rand-int-in-range 10 50)))
                    (if (>= (robot.battery r) 0.9)
                        (robot.navigate_to r (rand-int-in-range 10 50) (rand-int-in-range 10 50)))))
            (dumber_robot r))))";

#[derive(Default, Debug)]
pub struct CtxDumber {}

impl GetModule for CtxDumber {
    fn get_module(self) -> Module {
        Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_DUMBER_ROBOT].into(),
            label: MOD_DUMBER.to_string(),
        }
    }
}

impl Documentation for CtxDumber {
    fn documentation() -> Vec<LHelp> {
        vec![LHelp::new(MOD_DUMBER, DOC_MOD_DUMBER)]
    }
}
