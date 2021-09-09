//! Module to add an help module to the project.
//! It provides a struct for the help in Scheme

use crate::core::LEnv;
use crate::language::doc::*;
use crate::language::scheme_primitives::*;
use crate::structs::LError::{WrongNumberOfArgument, WrongType};
use crate::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_HELP: &str = "mod-help";
const DOC_MOD_HELP: &str =
    "Documentation of the module help. Add Documentation for core functions aswell.";
const DOC_MOD_HELP_VERBOSE: &str = "functions:\n\
                                    -help";

const HELP: &str = "help";
const DOC_HELP: &str =
    "Give a list of all the available functions added by the modules and available in the core.";
const DOC_HELP_VERBOSE: &str = "takes 0..1 arguments:\
                                -no argument: give the list of all the functions\n\
                                -1 argument: give the documentation of the function.";

/// Context of the module doc.
/// Store the help objects.
pub struct CtxDoc {
    map_help: BTreeMap<String, LHelp>,
    //BTreeMap is preferred to a simple HashMap, because entries will be ordered, and it will be easier to debug.
    //map_help: HashMap<String, LHelp>,
}

impl Default for CtxDoc {
    fn default() -> Self {
        let mut ctx = CtxDoc {
            map_help: Default::default(),
        };
        ctx.insert_doc(CtxDoc::documentation());
        ctx
    }
}

impl CtxDoc {
    pub fn insert_doc(&mut self, helps: Vec<LHelp>) {
        for element in helps {
            self.map_help.insert(element.label.into(), element);
        }
    }
}

pub trait Documentation {
    fn documentation() -> Vec<LHelp>;
}

/// Trait to implement Lisp Documentation.
/// Add symbols to help
impl Documentation for CtxDoc {
    /// Documentation for context CtxDoc
    /// Two elements:
    /// - mod-help
    /// - help
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(MOD_HELP, DOC_MOD_HELP, DOC_MOD_HELP_VERBOSE),
            LHelp::new_verbose(HELP, DOC_HELP, DOC_HELP_VERBOSE),
            LHelp::new(DEFINE, DOC_DEFINE),
            LHelp::new_verbose(LAMBDA, DOC_LAMBDA, DOC_LAMBDA_VEBROSE),
            LHelp::new(DEF_MACRO, DOC_DEF_MACRO),
            LHelp::new(IF, DOC_IF),
            LHelp::new(QUOTE, DOC_QUOTE),
            LHelp::new(QUASI_QUOTE, QUASI_QUOTE),
            LHelp::new(UNQUOTE, DOC_UNQUOTE),
            LHelp::new(SET, DOC_SET),
            LHelp::new_verbose(BEGIN, DOC_BEGIN, DOC_BEGIN_VERBOSE),
            LHelp::new(LIST, DOC_LIST),
            LHelp::new_verbose(MAP, DOC_MAP, DOC_MAP_VERBOSE),
            LHelp::new(GET, DOC_GET),
            LHelp::new(CAR, DOC_CAR),
            LHelp::new(CDR, DOC_CDR),
            LHelp::new(APPEND, DOC_APPEND),
            LHelp::new(MEMBER, DOC_MEMBER),
            LHelp::new(LAST, DOC_LAST),
            LHelp::new(EMPTY, DOC_EMPTY),
            LHelp::new(LEN, DOC_LEN),
            LHelp::new(REVERSE, DOC_REVERSE),
            LHelp::new(CONS, DOC_CONS),
            LHelp::new_verbose(GET_MAP, DOC_GET_MAP, DOC_GET_MAP_VERBOSE),
            LHelp::new_verbose(SET_MAP, DOC_SET_MAP, DOC_SET_MAP_VERBOSE),
            LHelp::new(ADD, DOC_ADD),
            LHelp::new(SUB, DOC_SUB),
            LHelp::new(MUL, DOC_MUL),
            LHelp::new(DIV, DOC_DIV),
            LHelp::new(GT, DOC_GT),
            LHelp::new(GE, DOC_GE),
            LHelp::new(LT, DOC_LT),
            LHelp::new(LE, DOC_LE),
            LHelp::new(EQ, DOC_EQ),
            LHelp::new(IS_NIL, DOC_IS_NIL),
            LHelp::new(IS_NUMBER, DOC_IS_NUMBER),
            LHelp::new(IS_BOOL, DOC_IS_BOOL),
            LHelp::new(IS_SYMBOL, DOC_IS_SYMBOL),
            LHelp::new(IS_FN, DOC_IS_FN),
            LHelp::new(IS_MUT_FN, DOC_IS_MUT_FN),
            LHelp::new(IS_MAP, DOC_IS_MAP),
            LHelp::new(IS_LIST, DOC_IS_LIST),
            LHelp::new(IS_LAMBDA, DOC_IS_LAMBDA),
            LHelp::new(IS_QUOTE, DOC_IS_QUOTE),
            LHelp::new(IS_PAIR, DOC_IS_PAIR),
            LHelp::new(IS_EQUAL, DOC_IS_EQUAL),
            LHelp::new(AWAIT, DOC_AWAIT),
            LHelp::new(ASYNC, DOC_ASYNC),
            LHelp::new(EVAL, DOC_EVAL),
            LHelp::new(LET, DOC_LET),
            LHelp::new(LET_STAR, DOC_LET_STAR),
            LHelp::new(MACRO_EXPAND, DOC_MACRO_EXPAND),
        ]
    }
}

impl CtxDoc {
    pub fn get_all(&self) -> String {
        let mut string = String::new();
        for element in self.map_help.iter() {
            string.push_str(format!("¤ {}\n", element.1).as_str())
        }
        string
    }

    pub fn get(&self, sym: &str) -> String {
        match self.map_help.get(sym) {
            None => "no such function".to_string(),
            Some(h) => format!("{:?}\n", h),
        }
    }
}

impl GetModule for CtxDoc {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_HELP.into(),
        };

        module.add_fn_prelude(HELP, help);
        module
    }
}

//TODO: doc
#[derive(Clone)]
pub struct LHelp {
    label: &'static str,
    short: &'static str,
    verbose: Option<&'static str>,
}

impl LHelp {
    pub fn new(label: &'static str, short: &'static str) -> LHelp {
        LHelp {
            label,
            short,
            verbose: None,
        }
    }

    pub fn new_verbose(label: &'static str, short: &'static str, verbose: &'static str) -> LHelp {
        LHelp {
            label,
            short,
            verbose: Some(verbose),
        }
    }
}

impl Debug for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}\n{}", self.short, self.verbose.unwrap_or(""))
    }
}

impl Display for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:<20}: {}", self.label, self.short)
    }
}

///print the help
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of
pub fn help(args: &[LValue], _: &LEnv, ctx: &CtxDoc) -> Result<LValue, LError> {
    match args.len() {
        0 => Ok(ctx.get_all().into()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(LValue::String(ctx.get(fun.get_label()))),
            LValue::MutFn(fun) => Ok(LValue::String(ctx.get(fun.get_label()))),
            LValue::Symbol(s) => Ok(LValue::String(ctx.get(s))),
            LValue::CoreOperator(co) => Ok(LValue::String(ctx.get(&co.to_string()))),
            lv => Err(WrongType(
                HELP,
                lv.clone(),
                lv.into(),
                NameTypeLValue::Symbol,
            )),
        },
        _ => Err(WrongNumberOfArgument(HELP, args.into(), args.len(), 0..1)),
    }
}
