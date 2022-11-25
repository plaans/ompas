use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;

/*
LANGUAGE
 */

const MOD_STRING: &str = "string";
const CONCATENATE: &str = "concatenate";
//todo
const DOC_CONCATENATE: &str = "todo!";

#[derive(Default)]
pub struct CtxString {}

impl IntoModule for CtxString {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(()),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_STRING.to_string(),
        };

        module.add_fn_prelude(CONCATENATE, concatenate);
        module
    }

    fn documentation(&self) -> Documentation {
        vec![LHelp::new(CONCATENATE, DOC_CONCATENATE)].into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

//Todo: add test for concatenate
#[scheme_fn]
pub fn concatenate(strings: Vec<String>) -> String {
    let mut str = String::new();
    for e in strings {
        str.push_str(e.to_string().as_str())
    }
    str
}
