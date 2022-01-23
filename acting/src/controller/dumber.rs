//! [Deprecated]
//! Previous work to develop a dumb controller to test lisp integration

use ompas_lisp::core::structs::documentation::{Documentation, LHelp};
use ompas_lisp::core::structs::module::{IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;
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

impl IntoModule for CtxDumber {
    fn into_module(self) -> Module {
        Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_DUMBER_ROBOT].into(),
            label: MOD_DUMBER.to_string(),
        }
    }

    fn documentation(&self) -> Documentation {
        vec![LHelp::new(MOD_DUMBER, DOC_MOD_DUMBER)].into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}
