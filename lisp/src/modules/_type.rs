//! Scheme module implementing symbol typing.
//! Development in standby. Some features might not be completely working...
use crate::core::language::*;
use crate::core::structs::contextcollection::Context;
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror;
use crate::core::structs::lerror::LError::{
    ConversionError, NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use crate::core::structs::lerror::LResult;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{IntoModule, Module};
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;
use std::convert::TryInto;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, RwLock};

//pub const TYPE: &str = "type";
//pub const STATE: &str = "state";
//pub const IS_PAIR: &str = "pair?";

const MOD_TYPE: &str = "type";
const DOC_MOD_TYPE: &str = "Module to define types for symbols. You can define state functions, or custom types and sub-types.";
const DOC_MOD_TYPE_VERBOSE: &str = "functions: \n\
                                   \t-type?\n
                                   \t-sf?\n\
                                   \t-obj?\n\
                                   \t-state-function\n\
                                   \t-type-of\n\
                                   \t-sub-type\n\
                                   \t-get-type\n\
                                   \t-new-sf\n\
                                   \t-new-obj\n";

//Verification

const IS_TYPE: &str = "type?";
const IS_STATE_FUNCTION: &str = "sf?";
const IS_OBJECT: &str = "obj?";

//FactBase language
const STATE_FUNCTION: &str = "state-function";

//basic types
const INDEX_TYPE_INT: usize = 0;
const INDEX_TYPE_FLOAT: usize = 1;
const INDEX_TYPE_USIZE: usize = 2;
const INDEX_TYPE_OBJECT: usize = 3;
const INDEX_TYPE_BOOL: usize = 4;

const TYPE_OF: &str = "type-of";
const SUB_TYPE: &str = "sub-type";
const GET_TYPE: &str = "get-type";
const NEW_STATE_FUNCTION: &str = "new-sf";
const NEW_OBJECT: &str = "new-obj";

#[derive(Clone, Debug)]
pub enum LSymType {
    StateFunction(LStateFunction),
    Type(Option<usize>),
    Object(usize),
}

impl Display for LSymType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LSymType::StateFunction(sf) => write!(f, "{}", sf),
            LSymType::Type(t) => match t {
                None => write!(f, "root type"),
                Some(_type) => write!(f, "subtype of type_id : {}", _type),
            },
            LSymType::Object(o) => write!(f, "type_id: {}", o),
        }
    }
}

impl LSymType {
    pub fn as_state_function(&self) -> lerror::Result<LStateFunction> {
        match self {
            LSymType::StateFunction(sf) => Ok(sf.clone()),
            lst => Err(ConversionError(
                "as_state_function",
                lst.into(),
                TypeLValue::Other(STATE_FUNCTION.to_string()),
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LType {
    Int,
    Bool,
    Usize,
    Float,
    Object,
    Symbol(String),
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LType::Int => write!(f, "int"),
            LType::Bool => write!(f, "bool"),
            LType::Float => write!(f, "float"),
            LType::Symbol(s) => write!(f, "{}", s),
            LType::Object => write!(f, "object"),
            LType::Usize => write!(f, "usize"),
        }
    }
}

impl From<&str> for LType {
    fn from(s: &str) -> Self {
        match s {
            INT => LType::Int,
            FLOAT => LType::Float,
            OBJECT => LType::Object,
            BOOL => LType::Bool,
            USIZE => LType::Usize,
            str => LType::Symbol(str.into()),
        }
    }
}

impl PartialEq for &LSymType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LSymType::Object(o1), LSymType::Object(o2)) => o1 == o2,
            (LSymType::Type(t1), LSymType::Type(t2)) => t1 == t2,
            (LSymType::StateFunction(sf1), LSymType::StateFunction(sf2)) => sf1 == sf2,
            _ => false,
        }
    }
}

impl From<String> for LType {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

impl From<&LType> for String {
    fn from(lt: &LType) -> Self {
        match lt {
            LType::Int => INT.into(),
            LType::Bool => BOOL.into(),
            LType::Float => FLOAT.into(),
            LType::Symbol(s) => s.clone(),
            LType::Object => OBJECT.into(),
            LType::Usize => USIZE.into(),
        }
    }
}

impl From<LType> for String {
    fn from(lt: LType) -> Self {
        (&lt).into()
    }
}

impl From<LSymType> for TypeLValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl From<&LSymType> for LValue {
    fn from(lst: &LSymType) -> Self {
        LValue::Symbol(lst.to_string())
    }
}

impl From<LSymType> for LValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl From<&LSymType> for TypeLValue {
    fn from(lst: &LSymType) -> Self {
        match lst {
            LSymType::StateFunction(_) => TypeLValue::Other("State Function".to_string()),
            LSymType::Type(_) => TypeLValue::Other("TYPE".to_string()),
            LSymType::Object(_) => TypeLValue::Other("Object".to_string()),
        }
    }
}

impl PartialEq for LType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LType::Int, LType::Int) => true,
            (LType::Bool, LType::Bool) => true,
            (LType::Float, LType::Float) => true,
            (LType::Symbol(s1), LType::Symbol(s2)) => s1 == s2,
            (_, _) => false,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct LStateFunction {
    pub t_params: Vec<String>,
    pub t_value: String,
}

impl Display for LStateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut sr = String::new();
        sr.push('(');
        for (i, t_param) in self.t_params.iter().enumerate() {
            sr.push_str(t_param.to_string().as_str());
            if i > 0 {
                sr.push(',');
            }
        }
        sr.push_str(format!(") = {}", self.t_value).as_str());
        write!(f, "{}", sr)
    }
}

#[derive(Debug)]
pub struct CtxType {
    map_sym_type_id: Arc<RwLock<im::HashMap<String, usize>>>,
    map_type_id_sym: Arc<RwLock<im::HashMap<usize, String>>>,
    types: Arc<RwLock<Vec<LSymType>>>,
}

impl Default for CtxType {
    fn default() -> Self {
        let types = vec![LSymType::Type(None); 5];

        let mut map_sym_type_id: im::HashMap<String, usize> = Default::default();
        map_sym_type_id.insert(INT.into(), INDEX_TYPE_INT);
        map_sym_type_id.insert(FLOAT.into(), INDEX_TYPE_FLOAT);
        map_sym_type_id.insert(USIZE.into(), INDEX_TYPE_USIZE);
        map_sym_type_id.insert(BOOL.into(), INDEX_TYPE_BOOL);
        map_sym_type_id.insert(OBJECT.into(), INDEX_TYPE_OBJECT);

        let mut map_type_id_sym: im::HashMap<usize, String> = Default::default();
        map_type_id_sym.insert(INDEX_TYPE_INT, INT.into());
        map_type_id_sym.insert(INDEX_TYPE_FLOAT, FLOAT.into());
        map_type_id_sym.insert(INDEX_TYPE_USIZE, USIZE.into());
        map_type_id_sym.insert(INDEX_TYPE_BOOL, BOOL.into());
        map_type_id_sym.insert(INDEX_TYPE_OBJECT, OBJECT.into());

        Self {
            map_sym_type_id: Arc::new(RwLock::new(map_sym_type_id)),
            map_type_id_sym: Arc::new(RwLock::new(map_type_id_sym)),
            types: Arc::new(RwLock::new(types)),
        }
    }
}

impl CtxType {
    pub fn get_type_id(&self, sym: &str) -> Option<usize> {
        match self.map_sym_type_id.read() {
            Ok(map) => map.get(sym).cloned(),
            Err(e) => panic!("{}", e),
        }
    }

    pub fn get_sym(&self, type_id: &usize) -> Option<String> {
        self.map_type_id_sym.read().unwrap().get(type_id).cloned()
    }

    pub fn get_type(&self, type_id: &usize) -> Option<LSymType> {
        self.types.read().unwrap().get(*type_id).cloned()
    }

    pub fn get_type_from_sym(&self, sym: &str) -> Option<LSymType> {
        match self.get_type_id(sym) {
            None => None,
            Some(type_id) => self.get_type(&type_id),
        }
    }

    pub fn bind_sym_type(&self, sym: &str, type_id: usize) {
        self.map_sym_type_id
            .write()
            .unwrap()
            .insert(sym.to_string(), type_id);
        self.map_type_id_sym
            .write()
            .unwrap()
            .insert(type_id, sym.to_string());
    }

    pub fn add_type(&self, sym_type: LSymType) -> usize {
        self.types.write().unwrap().push(sym_type);
        self.types.write().unwrap().len() - 1
    }
}

impl IntoModule for CtxType {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_TYPE.into(),
        };

        module.add_fn_prelude(IS_STATE_FUNCTION, is_state_function);
        module.add_fn_prelude(IS_OBJECT, is_object);
        module.add_fn_prelude(IS_TYPE, is_type);
        module.add_fn_prelude(GET_TYPE, get_type);
        module.add_fn_prelude(TYPE_OF, type_of);
        module.add_fn_prelude(SUB_TYPE, sub_type);
        module.add_fn_prelude(NEW_STATE_FUNCTION, new_state_function);
        module.add_fn_prelude(NEW_OBJECT, new_object);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new(MOD_TYPE, DOC_MOD_TYPE),
            LHelp::new(IS_STATE_FUNCTION, DOC_IS_STATE_FUNCTION),
            LHelp::new(IS_OBJECT, DOC_IS_OBJECT),
            LHelp::new(IS_TYPE, DOC_IS_TYPE),
            LHelp::new(GET_TYPE, DOC_GET_TYPE),
            LHelp::new(TYPE_OF, DOC_TYPE_OF),
            LHelp::new_verbose(SUB_TYPE, DOC_SUB_TYPE, DOC_SUB_TYPE_VERBOSE),
            LHelp::new_verbose(
                NEW_STATE_FUNCTION,
                DOC_NEW_STATE_FUNCTION,
                DOC_NEW_STATE_FUNCTION_VERBOSE,
            ),
            LHelp::new_verbose(NEW_OBJECT, DOC_NEW_OBJECT, DOC_NEW_OBJECT_VERBOSE),
            LHelp::new_verbose(MOD_TYPE, DOC_MOD_TYPE, DOC_MOD_TYPE_VERBOSE),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

/*
DOCUMENTATION
 */

const DOC_IS_STATE_FUNCTION: &str = "Return true if symbol is state function";
const DOC_IS_OBJECT: &str = "Return true if symbol is object";

const DOC_IS_TYPE: &str = "Return true if symbol is type";

const DOC_GET_TYPE: &str = "Return type of a typed symbol";
const DOC_TYPE_OF: &str = "Associate a type to a symbol";
const DOC_SUB_TYPE: &str = "Create a sub-type of a type.";
const DOC_SUB_TYPE_VERBOSE: &str = "Takes one argument that is the type it inherits.\n\
                                    Example: (typeof robot (subtype object))";
const DOC_NEW_STATE_FUNCTION: &str =
    "Create a new state-function with a list of parameters types a value type.";
const DOC_NEW_STATE_FUNCTION_VERBOSE: &str = "Takes 2+ arguments.\
                                       \nExample: (typeof loc (new-state-function robot place))";
const DOC_NEW_OBJECT: &str = "Create a new typed object";
const DOC_NEW_OBJECT_VERBOSE: &str = "Takes one argument that is the type of the object \n\
                                      Example: (typeof bob robot)";

/*
FUNCTIONS
 */

pub fn is_type(args: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;
    match args.len() {
        1 => match &args[0] {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => Err(SpecialError(IS_TYPE, "symbol has no type".to_string())),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(
                IS_TYPE,
                lv.clone(),
                lv.into(),
                TypeLValue::Symbol,
            )),
        },
        i => Err(WrongNumberOfArgument(IS_TYPE, args.into(), i, 1..1)),
    }
}

pub fn is_object(args: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(
                IS_OBJECT,
                lv.clone(),
                lv.into(),
                TypeLValue::Symbol,
            )),
        },
        i => Err(WrongNumberOfArgument(IS_OBJECT, args.into(), i, 1..1)),
    }
}

pub fn is_state_function(args: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;

    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(
                IS_STATE_FUNCTION,
                lv.clone(),
                lv.into(),
                TypeLValue::Symbol,
            )),
        },
        i => Err(WrongNumberOfArgument(
            IS_STATE_FUNCTION,
            args.into(),
            i,
            1..1,
        )),
    }
}

pub fn type_of(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            TYPE_OF,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;

    match &args[0] {
        LValue::Symbol(s) => {
            let lv = &args[1];
            match lv {
                LValue::Number(LNumber::Usize(u)) => {
                    ctx.bind_sym_type(s, *u);
                    Ok(LValue::Nil)
                }
                lv => Err(WrongType(
                    TYPE_OF,
                    lv.clone(),
                    lv.into(),
                    TypeLValue::Other("type_id(usize)".to_string()),
                )),
            }
        }
        lv => Err(WrongType(
            TYPE_OF,
            lv.clone(),
            lv.into(),
            TypeLValue::Symbol,
        )),
    }
}

pub fn sub_type(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            SUB_TYPE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;

    let expected_type = TypeLValue::Other("TYPE".to_string());
    let parent_type: usize = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => {
                return Err(SpecialError(
                    SUB_TYPE,
                    format!("{} has no type annotations", s),
                ))
            }
            Some(lst) => match lst {
                LSymType::Type(_) => ctx.get_type_id(s).unwrap(),
                lst => {
                    return Err(WrongType(
                        SUB_TYPE,
                        lst.clone().into(),
                        lst.into(),
                        expected_type,
                    ))
                }
            },
        },
        lv => {
            return Err(WrongType(
                SUB_TYPE,
                lv.clone(),
                lv.into(),
                TypeLValue::Symbol,
            ))
        }
    };

    let type_id = ctx.add_type(LSymType::Type(Some(parent_type)));
    Ok(type_id.into())
}

pub fn new_state_function(args: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;

    let mut t_params: Vec<String> = Vec::new();
    let mut t_value: String = String::from(OBJECT);
    let expected_type = TypeLValue::Other("TYPE".to_string());
    for (i, arg) in args.iter().enumerate() {
        match arg {
            LValue::Symbol(s) => {
                if is_type(&args[i..i + 1], env)?.try_into()? {
                    if i == args.len() - 1 {
                        t_value = s.clone();
                    } else {
                        t_params.push(s.clone())
                    }
                } else {
                    return match ctx.get_type_from_sym(s) {
                        None => Err(WrongType(
                            NEW_STATE_FUNCTION,
                            arg.clone(),
                            arg.into(),
                            expected_type,
                        )),
                        Some(lst) => Err(WrongType(
                            NEW_STATE_FUNCTION,
                            arg.clone(),
                            lst.into(),
                            expected_type,
                        )),
                    };
                }
            }
            lv => {
                return Err(WrongType(
                    NEW_STATE_FUNCTION,
                    lv.clone(),
                    lv.into(),
                    expected_type,
                ))
            }
        }
    }

    let type_id = ctx.add_type(LSymType::StateFunction(LStateFunction {
        t_params,
        t_value,
    }));
    Ok(type_id.into())
}

pub fn new_object(args: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            NEW_OBJECT,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let type_id;
    let option_type = match &args[0] {
        LValue::Symbol(s) => {
            type_id = match ctx.get_type_id(s) {
                None => {
                    return Err(SpecialError(
                        NEW_OBJECT,
                        "no type annotation corresponding to the symbol".to_string(),
                    ))
                }
                Some(u) => u,
            };
            ctx.get_type(&type_id)
        }
        LValue::Number(LNumber::Usize(u)) => {
            type_id = *u;
            ctx.get_type(u)
        }
        lv => {
            return Err(NotInListOfExpectedTypes(
                NEW_OBJECT,
                lv.clone(),
                lv.into(),
                vec![TypeLValue::Symbol, TypeLValue::Usize],
            ))
        }
    };

    match option_type {
        None => Err(WrongType(
            NEW_OBJECT,
            (&args[0]).clone(),
            (&args[0]).into(),
            TypeLValue::Symbol,
        )),
        Some(lst) => match &lst {
            LSymType::Type(_) => {
                let type_id = ctx.add_type(LSymType::Object(type_id));
                Ok(type_id.into())
            }
            lst => Err(WrongType(
                NEW_OBJECT,
                lst.clone().into(),
                lst.into(),
                TypeLValue::Other("type".to_string()),
            )),
        },
    }
}

pub fn get_type(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            GET_TYPE,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let ctx = env.get_context::<CtxType>(MOD_TYPE)?;

    let type_as_string = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => SYMBOL.to_string(),
            Some(lst) => match &lst {
                LSymType::Object(u) => ctx.get_sym(u).unwrap(),
                LSymType::Type(parent_type) => match parent_type {
                    None => "root type".to_string(),
                    Some(u) => format!("subtype of {}", ctx.get_sym(u).unwrap()),
                },
                lst => lst.to_string(),
            },
        },
        lv => TypeLValue::from(lv).to_string(),
    };

    Ok(type_as_string.into())
}
