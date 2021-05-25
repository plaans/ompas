/*
Module to add an help module to the project.
It provides a struct for the help
 */

use im::HashMap;
use ompas_lisp::core::LEnv;
use ompas_lisp::language::*;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::fmt::{Debug, Display, Formatter};

/*
LANGUAGE
 */

const MOD_HELP: &str = "mod-help";
const DOC_MOD_HELP: &str =
    "Documentation of the module help. Add Documentation for core functions aswell";
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
    map_help: HashMap<String, LHelp>,
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
            LHelp::new(MOD_HELP, DOC_MOD_HELP, Some(DOC_MOD_HELP_VERBOSE)),
            LHelp::new(HELP, DOC_HELP, Some(DOC_HELP_VERBOSE)),
            LHelp::new(DEFINE, DOC_DEFINE, None),
            LHelp::new(LAMBDA, DOC_LAMBDA, Some(DOC_LAMBDA_VEBROSE)),
            LHelp::new(DEF_MACRO, DOC_DEF_MACRO, None),
            LHelp::new(IF, DOC_IF, None),
            LHelp::new(QUOTE, DOC_QUOTE, None),
            LHelp::new(QUASI_QUOTE, QUASI_QUOTE, None),
            LHelp::new(UNQUOTE, DOC_UNQUOTE, None),
            LHelp::new(SET, DOC_SET, None),
            LHelp::new(BEGIN, DOC_BEGIN, Some(DOC_BEGIN_VERBOSE)),
            LHelp::new(LIST, DOC_LIST, None),
            LHelp::new(MAP, DOC_MAP, Some(DOC_MAP_VERBOSE)),
            LHelp::new(GET, DOC_GET, None),
            LHelp::new(CAR, DOC_CAR, None),
            LHelp::new(CDR, DOC_CDR, None),
            LHelp::new(APPEND, DOC_APPEND, None),
            LHelp::new(MEMBER, DOC_MEMBER, None),
            LHelp::new(LAST, DOC_LAST, None),
            LHelp::new(EMPTY, DOC_EMPTY, None),
            LHelp::new(LEN, DOC_LEN, None),
            LHelp::new(REVERSE, DOC_REVERSE, None),
            LHelp::new(CONS, DOC_CONS, None),
            LHelp::new(GET_MAP, DOC_GET_MAP, Some(DOC_GET_MAP_VERBOSE)),
            LHelp::new(SET_MAP, DOC_SET_MAP, Some(DOC_SET_MAP_VERBOSE)),
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
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_HELP,
        };

        module.add_fn_prelude(HELP, Box::new(help));
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
    pub fn new(label: &'static str, short: &'static str, verbose: Option<&'static str>) -> LHelp {
        LHelp {
            label,
            short,
            verbose,
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
            LValue::Fn(fun) => Ok(ctx.get(&fun.get_label()).into()),
            LValue::MutFn(fun) => Ok(ctx.get(&fun.get_label()).into()),
            LValue::Symbol(s) => Ok(ctx.get(s).into()),
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        _ => Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    }
}
