use crate::core::language::*;
use crate::core::root_module::basic_math::language::*;
use crate::core::root_module::basic_math::*;
use crate::core::root_module::language::*;
use crate::core::root_module::list::language::*;
use crate::core::root_module::list::*;
use crate::core::root_module::map::language::*;
use crate::core::root_module::map::*;
use crate::core::root_module::predicate::language::*;
use crate::core::root_module::predicate::*;
use crate::core::structs::contextcollection::Context;
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::{NotInListOfExpectedTypes, WrongNumberOfArgument};
use crate::core::structs::lerror::LResult;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{IntoModule, Module};
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;

pub mod basic_math;
pub mod env;
pub mod list;
pub mod map;
pub mod predicate;

pub mod language {
    use crate::core::language::{LIST, MAP};
    use crate::core::root_module::basic_math::language::*;
    use crate::core::root_module::list::language::*;
    use crate::core::root_module::map::language::*;
    use crate::core::root_module::predicate::language::*;
    use crate::core::structs::lcoreoperator::language::*;

    pub const MOD_ROOT: &str = "mod-root";

    pub const SET: &str = "set";
    pub const GET: &str = "get";

    pub fn get_pure_primitives() -> Vec<&'static str> {
        vec![
            FIRST,
            SECOND,
            THIRD,
            REST,
            CAR,
            CDR,
            APPEND,
            LAST,
            EMPTY,
            LEN,
            MEMBER,
            REVERSE,
            LIST,
            GET_LIST,
            SET_LIST,
            CONS,
            SET,
            GET,
            GET_MAP,
            SET_MAP,
            REMOVE_MAP,
            REMOVE_KEY_VALUE_MAP,
            UNION_MAP,
            MAP,
            NOT,
            ADD,
            SUB,
            MUL,
            DIV,
            GT,
            LT,
            GE,
            LE,
            EQ,
            IS_NUMBER,
            IS_FLOAT,
            IS_INT,
            IS_BOOL,
            IS_SYMBOL,
            IS_STRING,
            IS_FN,
            IS_MUT_FN,
            IS_LIST,
            IS_MAP,
            IS_LAMBDA,
            IS_QUOTE,
            IS_PAIR,
            IS_EQUAL,
            IS_NIL,
        ]
    }

    pub fn get_scheme_primitives() -> Vec<&'static str> {
        let mut vec = get_pure_primitives();
        vec.append(&mut vec![
            DEFINE,
            DEF_MACRO,
            LAMBDA,
            IF,
            QUOTE,
            QUASI_QUOTE,
            UNQUOTE,
            "'", //quote char
            ",", //quasi quote char
            "`", //unquote char
            BEGIN,
            ASYNC,
            AWAIT,
            EVAL,
            PARSE,
            EXPAND,
        ]);

        vec
    }
}

#[derive(Default)]
pub struct CtxRoot(());

impl IntoModule for CtxRoot {
    /// Returns all basic functions, macros, and lambdas
    ///
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_ROOT.into(),
        };

        //Special entry
        module.add_fn_prelude(GET, get);
        module.add_fn_prelude(SET, set);
        //State is an alias for map

        /*
         * LIST FUNCTIONS
         */
        module.add_fn_prelude(LIST, list);
        module.add_fn_prelude(FIRST, first);
        module.add_fn_prelude(SECOND, second);
        module.add_fn_prelude(THIRD, third);
        module.add_fn_prelude(REST, rest);
        module.add_fn_prelude(CAR, car);
        module.add_fn_prelude(CDR, cdr);
        module.add_fn_prelude(LAST, last);
        module.add_fn_prelude(CONS, cons);
        module.add_fn_prelude(LEN, length);
        module.add_fn_prelude(EMPTY, empty);
        module.add_fn_prelude(GET_LIST, get_list);
        module.add_fn_prelude(SET_LIST, set_list);

        //Map functions
        module.add_fn_prelude(MAP, map);
        module.add_fn_prelude(GET_MAP, get_map);
        module.add_fn_prelude(SET_MAP, set_map);
        module.add_fn_prelude(UNION_MAP, union_map);
        module.add_fn_prelude(REMOVE_MAP, remove_map);
        module.add_fn_prelude(REMOVE_KEY_VALUE_MAP, remove_key_value_map);

        module.add_fn_prelude(NOT, not);
        module.add_fn_prelude(NOT_SHORT, not);

        module.add_fn_prelude(APPEND, append);

        module.add_fn_prelude(MEMBER, member);

        module.add_fn_prelude(REVERSE, reverse);

        module.add_fn_prelude(ADD, add);
        module.add_fn_prelude(SUB, sub);
        module.add_fn_prelude(MUL, mul);
        module.add_fn_prelude(DIV, div);
        module.add_fn_prelude(GT, gt);
        module.add_fn_prelude(GE, ge);
        module.add_fn_prelude(LT, lt);
        module.add_fn_prelude(LE, le);
        module.add_fn_prelude(EQ, eq);

        //predicates
        module.add_fn_prelude(IS_INT, is_integer);
        module.add_fn_prelude(IS_FLOAT, is_float);
        module.add_fn_prelude(IS_NIL, is_nil);
        module.add_fn_prelude(IS_NUMBER, is_number);
        module.add_fn_prelude(IS_BOOL, is_bool);
        module.add_fn_prelude(IS_SYMBOL, is_symbol);
        module.add_fn_prelude(IS_STRING, is_string);
        module.add_fn_prelude(IS_FN, is_fn);
        //module.add_fn_prelude(IS_QUOTE, is_quote);
        module.add_fn_prelude(IS_MAP, is_map);
        module.add_fn_prelude(IS_LIST, is_list);
        module.add_fn_prelude(IS_LAMBDA, is_lambda);

        module.add_fn_prelude(IS_PAIR, is_pair);
        module.add_fn_prelude(IS_EQUAL, is_equal);
        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new(LIST, DOC_LIST),
            LHelp::new_verbose(MAP, DOC_MAP, DOC_MAP_VERBOSE),
            LHelp::new(GET, DOC_GET),
            LHelp::new(FIRST, DOC_FIRST),
            LHelp::new(SECOND, DOC_SECOND),
            LHelp::new(THIRD, DOC_THIRD),
            LHelp::new(REST, DOC_REST),
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
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}
/// Default function of the Lisp Environement.
/// Does nothing outside returning a string.
pub fn default(_args: &[LValue], _: &LEnv) -> LResult {
    Ok(LValue::String("default function".to_string()))
}

/// Construct a map

pub fn set(args: &[LValue], env: &LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            SET,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }
    match &args[0] {
        LValue::Map(_) => set_map(args, env),
        LValue::List(_) | LValue::Nil => set_list(args, env),
        _ => Err(NotInListOfExpectedTypes(
            SET,
            args[0].clone(),
            (&args[0]).into(),
            vec![TypeLValue::List, TypeLValue::Map, TypeLValue::Nil],
        )),
    }
}

pub fn get(args: &[LValue], env: &LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            GET,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }
    match &args[0] {
        LValue::Map(_) => get_map(args, env),
        LValue::List(_) | LValue::Nil => get_list(args, env),
        _ => Err(NotInListOfExpectedTypes(
            GET,
            args[0].clone(),
            (&args[0]).into(),
            vec![TypeLValue::List, TypeLValue::Map, TypeLValue::Nil],
        )),
    }
}

/// return the length of the object if it is a table or a list.
pub fn length(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(LEN, args.into(), args.len(), 1..1).into());
    }

    match &args[0] {
        LValue::List(l) => Ok(l.len().into()),
        LValue::Map(m) => Ok(m.len().into()),
        LValue::Nil => Ok(0.into()),
        lv => Err(NotInListOfExpectedTypes(
            LEN,
            lv.clone(),
            lv.into(),
            vec![TypeLValue::List, TypeLValue::Map],
        )),
    }
}
/// Returns true if a hashmap or list is empty
pub fn empty(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(EMPTY, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::List(l) => Ok(l.is_empty().into()),
        LValue::Map(m) => Ok(m.is_empty().into()),
        LValue::Nil => Ok(true.into()),
        lv => Err(NotInListOfExpectedTypes(
            EMPTY,
            lv.clone(),
            lv.into(),
            vec![TypeLValue::List, TypeLValue::Map, TypeLValue::Nil],
        )),
    }
}
