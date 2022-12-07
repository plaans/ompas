use sompas_language::primitives::*;
use sompas_structs::lmodule::LModule;

#[derive(Default)]
pub struct ModPrimitive {}

impl From<ModPrimitive> for LModule {
    fn from(m: ModPrimitive) -> LModule {
        let mut module = LModule::new(m, MOD_PRIMITIVE, DOC_MOD_PRIMITIVE);
        module.add_doc(DEFINE, DOC_DEFINE);
        module.add_doc(DEF_MACRO, DOC_DEF_MACRO);
        module.add_doc(FN_LAMBDA, (DOC_FN_LAMBDA, DOC_FN_LAMBDA_VERBOSE));
        module.add_doc(IF, (DOC_IF, DOC_IF_VERBOSE));
        module.add_doc(QUOTE, DOC_QUOTE);
        module.add_doc(QUASI_QUOTE, DOC_QUASI_QUOTE);
        module.add_doc(DOC_UNQUOTE, DOC_UNQUOTE);
        module.add_doc(QUOTE_CHAR, DOC_QUOTE_CHAR);
        module.add_doc(QUASI_QUOTE_CHAR, DOC_QUASI_QUOTE_CHAR);
        module.add_doc(UNQUOTE_CHAR, DOC_UNQUOTE_CHAR);
        module.add_doc(BEGIN, (DOC_BEGIN, DOC_BEGIN_VERBOSE));
        module.add_doc(ASYNC, (DOC_ASYNC, DOC_ASYNC_VERBOSE));
        module.add_doc(AWAIT, (DOC_AWAIT, DOC_AWAIT_VERBOSE));
        module.add_doc(RACE, DOC_RACE);
        module.add_doc(ENR, DOC_ENR);
        module.add_doc(EVAL, DOC_EVAL);
        module.add_doc(PARSE, DOC_PARSE);
        module.add_doc(EXPAND, DOC_EXPAND);
        module.add_doc(DO, DOC_DO);
        module.add_doc(INTERRUPT, DOC_INTERRUPT);
        module.add_doc(UNINTERRUPTIBLE, DOC_UNINTERRUPTIBLE);
        module.add_doc(INTERRUPTIBLE, DOC_INTERRUPTIBLE);
        module.add_doc(UNINTERRUPTIBLE_SHORT, DOC_UNINTERRUPTIBLE_SHORT);
        module.add_doc(UNINTERRUPTIBLE_SHORT, DOC_INTERRUPTIBLE_SHORT);
        module
    }
}
