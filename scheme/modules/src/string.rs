use sompas_language::string::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;

/*
LANGUAGE
 */

#[derive(Default)]
pub struct ModString {}

impl From<ModString> for LModule {
    fn from(m: ModString) -> Self {
        let mut module = LModule::new(m, MOD_STRING, DOC_MOD_STRING);
        module.add_fn(CONCATENATE, concatenate, DOC_CONCATENATE, true);
        module
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
