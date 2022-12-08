use sompas_language::primitives::*;
use sompas_structs::lmodule::LModule;

#[derive(Default)]
pub struct ModPrimitive {}

pub const PRIMITIVE: &str = "Primitive";

impl From<ModPrimitive> for LModule {
    fn from(m: ModPrimitive) -> LModule {
        let mut module = LModule::new(m, MOD_PRIMITIVE, DOC_MOD_PRIMITIVE);
        module.add_doc(DEFINE, DOC_DEFINE, PRIMITIVE);
        module.add_doc(DEF_MACRO, DOC_DEF_MACRO, PRIMITIVE);
        module.add_doc(FN_LAMBDA, (DOC_FN_LAMBDA, DOC_FN_LAMBDA_VERBOSE), PRIMITIVE);
        module.add_doc(IF, (DOC_IF, DOC_IF_VERBOSE), PRIMITIVE);
        module.add_doc(QUOTE, DOC_QUOTE, PRIMITIVE);
        module.add_doc(QUASI_QUOTE, DOC_QUASI_QUOTE, PRIMITIVE);
        module.add_doc(DOC_UNQUOTE, DOC_UNQUOTE, PRIMITIVE);
        module.add_doc(QUOTE_CHAR, DOC_QUOTE_CHAR, PRIMITIVE);
        module.add_doc(QUASI_QUOTE_CHAR, DOC_QUASI_QUOTE_CHAR, PRIMITIVE);
        module.add_doc(UNQUOTE_CHAR, DOC_UNQUOTE_CHAR, PRIMITIVE);
        module.add_doc(BEGIN, (DOC_BEGIN, DOC_BEGIN_VERBOSE), PRIMITIVE);
        module.add_doc(ASYNC, (DOC_ASYNC, DOC_ASYNC_VERBOSE), PRIMITIVE);
        module.add_doc(AWAIT, (DOC_AWAIT, DOC_AWAIT_VERBOSE), PRIMITIVE);
        module.add_doc(RACE, DOC_RACE, PRIMITIVE);
        module.add_doc(ENR, DOC_ENR, PRIMITIVE);
        module.add_doc(EVAL, DOC_EVAL, PRIMITIVE);
        module.add_doc(PARSE, DOC_PARSE, PRIMITIVE);
        module.add_doc(EXPAND, DOC_EXPAND, PRIMITIVE);
        module.add_doc(DO, DOC_DO, PRIMITIVE);
        module.add_doc(INTERRUPT, DOC_INTERRUPT, PRIMITIVE);
        module.add_doc(UNINTERRUPTIBLE, DOC_UNINTERRUPTIBLE, PRIMITIVE);
        module.add_doc(INTERRUPTIBLE, DOC_INTERRUPTIBLE, PRIMITIVE);
        module.add_doc(UNINTERRUPTIBLE_SHORT, DOC_UNINTERRUPTIBLE_SHORT, PRIMITIVE);
        module.add_doc(UNINTERRUPTIBLE_SHORT, DOC_INTERRUPTIBLE_SHORT, PRIMITIVE);
        module
    }
}
