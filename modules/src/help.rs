/*
Module to add an help module to the project.
It provides a struct for the help
 */


use im::HashMap;
use aries_utils::input::Sym;
use ompas_lisp::core::RefLEnv;
use ompas_lisp::structs::{AsModule, Module, LValue, LError, NameTypeLValue};
use std::fmt::{Display, Formatter};
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use std::ops::Range;

/*
LANGUAGE
 */

pub const HELP: &str = "help";

#[derive(Default)]
pub struct CtxHelp {
    map_help: HashMap<Sym, LHelp>
}

impl CtxHelp {
   pub fn insert_helps(&mut self, helps: Vec<LHelp>) {
       for element in helps {
           self.map_help.insert(element.label.into(), element);
       }
   }
}

pub trait AsHelp {
    fn get_list_help() -> Vec<LHelp>;
}

impl AsHelp for CtxHelp {
    fn get_list_help() -> Vec<LHelp> {

        vec![LHelp {
            label: HELP,
            n_args: 0..1,
            description: None,
            e_description: None
        }]
    }
}

impl CtxHelp {
    pub fn get_all(&self) -> String {
        let mut string = String::new();
        for element in self.map_help.iter() {
            string.push_str(format!("- {}\n", element.1).as_str())
        }
        string
    }

    pub fn get(&self, sym: &Sym) -> String {
        match self.map_help.get(sym) {
            None => "no such function".to_string(),
            Some(h) => h.to_string(),
        }
    }
}

impl AsModule for CtxHelp {
    fn get_module() -> Module {

        let mut ctx = CtxHelp::default();
        ctx.insert_helps(CtxHelp::get_list_help());
        let mut module = Module {
            ctx: Box::new(ctx),
            prelude: vec![]
        };

        module.add_fn_prelude(HELP, Box::new(help));
        module
    }
}


//TODO: doc
#[derive(Debug, Clone)]
pub struct LHelp {
    label: &'static str,
    n_args: Range<usize>,
    description: Option<&'static str>,
    e_description: Option<&'static str>,
}

impl Display for LHelp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

///print the help
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of
pub fn help(args: &[LValue], _: &RefLEnv, ctx: &CtxHelp) -> Result<LValue, LError> {
    match args.len() {
        0 => {
            Ok(ctx.get_all().into())
        }
        1 => {
            match &args[0] {
                LValue::Symbol(s) => Ok(ctx.get(s).into()),
                lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
            }
        }
        _ => {
            Err(WrongNumberOfArgument(args.into(), args.len(), 0..1))
        }
    }
}