/*
Module to add an help module to the project.
It provides a struct for the help
 */

use aries_utils::input::Sym;
use im::HashMap;
use ompas_lisp::core::RefLEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{AsModule, LError, LValue, Module, NameTypeLValue};
use std::fmt::{Debug, Display, Formatter};

/*
LANGUAGE
 */

const MOD_HELP: &str = "mod-help";
const DOC_MOD_HELP: &str = "documentation of the module help";

const HELP: &str = "help";
const DESCRIPTION_HELP: &str =
    "Give a list of all the available functions added by the modules and available in the core.";

pub struct CtxDoc {
    map_help: HashMap<Sym, LHelp>,
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

impl Documentation for CtxDoc {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_HELP, DOC_MOD_HELP, None),
            LHelp::new(HELP, DESCRIPTION_HELP, None),
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

    pub fn get(&self, sym: &Sym) -> String {
        match self.map_help.get(sym) {
            None => "no such function".to_string(),
            Some(h) => format!("{:?}", h),
        }
    }
}

impl AsModule for CtxDoc {
    fn as_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
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
        write!(
            f,
            "{:<20} : {}\n {}",
            self.label,
            self.short,
            self.verbose.unwrap_or("")
        )
    }
}

impl Display for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:<20} : {}", self.label, self.short)
    }
}

///print the help
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of
pub fn help(args: &[LValue], _: &RefLEnv, ctx: &CtxDoc) -> Result<LValue, LError> {
    match args.len() {
        0 => Ok(ctx.get_all().into()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(ctx.get(&fun.get_label().into()).into()),
            LValue::MutFn(fun) => Ok(ctx.get(&fun.get_label().into()).into()),
            LValue::Symbol(s) => Ok(ctx.get(s).into()),
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        _ => Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    }
}
