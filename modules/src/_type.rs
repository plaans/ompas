use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;
use std::convert::TryInto;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

//pub const TYPE: &str = "type";
//pub const STATE: &str = "state";
//pub const IS_PAIR: &str = "pair?";

const MOD_TYPE: &str = "mod_type";
const DOC_MOD_TYPE: &str = "documentation of the module type";

//Verification

const IS_TYPE: &str = "type?";
const IS_STATE_FUNCTION: &str = "sf?";
const IS_OBJECT: &str = "obj?";

//FactBase language
const STATE_FUNCTION: &str = "state-function";

//basic types
const TYPE_INT: &str = "int";
const TYPE_FLOAT: &str = "float";
const TYPE_USIZE: &str = "usize";
const TYPE_OBJECT: &str = "object";
const TYPE_BOOL: &str = "boolean";

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
    pub fn as_state_function(&self) -> Result<LStateFunction, LError> {
        match self {
            LSymType::StateFunction(sf) => Ok(sf.clone()),
            lst => Err(LError::ConversionError(
                "as_state_function",
                lst.into(),
                NameTypeLValue::Other(STATE_FUNCTION.to_string()),
            )),
        }
    }
}

//TODO: remove this function
/*
impl AsLiteral for LSymType {
    fn as_command(&self) -> String {
        match self {
            LSymType::StateFunction(sf) => sf.as_command(),
            LSymType::Type(t) => match t {
                None => "".to_string(),
                Some(st) => {
                    format!("(subtype {})", st)
                }
            },
            LSymType::Object(o) => {
                format!("{}", o)
            }
        }
    }
}*/

//TODO: Finish implementation
/*impl Into<LValue> for LSymType {
    fn into(self) -> LValue {
        let string: String = match self {
            LSymType::StateFunction(sf) => sf.to_string(),
            LSymType::Type(t) => t.to_string(),
            LSymType::Object(o) => t.to_string(),
        };

        LValue::String(s)
    }
}*/

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
            TYPE_INT => LType::Int,
            TYPE_FLOAT => LType::Float,
            TYPE_OBJECT => LType::Object,
            TYPE_BOOL => LType::Bool,
            TYPE_USIZE => LType::Usize,
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
            LType::Int => TYPE_INT.into(),
            LType::Bool => TYPE_BOOL.into(),
            LType::Float => TYPE_FLOAT.into(),
            LType::Symbol(s) => s.clone(),
            LType::Object => TYPE_OBJECT.into(),
            LType::Usize => TYPE_USIZE.into(),
        }
    }
}

impl From<LType> for String {
    fn from(lt: LType) -> Self {
        (&lt).into()
    }
}

impl From<LSymType> for NameTypeLValue {
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

impl From<&LSymType> for NameTypeLValue {
    fn from(lst: &LSymType) -> Self {
        match lst {
            LSymType::StateFunction(_) => NameTypeLValue::Other("State Function".to_string()),
            LSymType::Type(_) => NameTypeLValue::Other("TYPE".to_string()),
            LSymType::Object(_) => NameTypeLValue::Other("Object".to_string()),
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

//TODO: remove this function
/*impl AsLiteral for LStateFunction {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", STATE_FUNCTION).as_str());
        for t_param in &self.t_params {
            result.push_str(format!("{} ", t_param.to_string()).as_str());
        }
        result.push_str(format!("{})\n", self.t_value.to_string()).as_str());
        result
    }
}*/

#[derive(Debug)]
pub struct CtxType {
    map_sym_type_id: im::HashMap<String, usize>,
    map_type_id_sym: im::HashMap<usize, String>,
    types: Vec<LSymType>,
}

//TODO: IMPROVE GET-TYPE and DEFAULT
impl Default for CtxType {
    fn default() -> Self {
        let types = vec![LSymType::Type(None); 5];

        let mut map_sym_type_id: im::HashMap<String, usize> = Default::default();
        map_sym_type_id.insert(TYPE_INT.into(), INDEX_TYPE_INT);
        map_sym_type_id.insert(TYPE_FLOAT.into(), INDEX_TYPE_FLOAT);
        map_sym_type_id.insert(TYPE_USIZE.into(), INDEX_TYPE_USIZE);
        map_sym_type_id.insert(TYPE_BOOL.into(), INDEX_TYPE_BOOL);
        map_sym_type_id.insert(TYPE_OBJECT.into(), INDEX_TYPE_OBJECT);

        let mut map_type_id_sym: im::HashMap<usize, String> = Default::default();
        map_type_id_sym.insert(INDEX_TYPE_INT, TYPE_INT.into());
        map_type_id_sym.insert(INDEX_TYPE_FLOAT, TYPE_FLOAT.into());
        map_type_id_sym.insert(INDEX_TYPE_USIZE, TYPE_USIZE.into());
        map_type_id_sym.insert(INDEX_TYPE_BOOL, TYPE_BOOL.into());
        map_type_id_sym.insert(INDEX_TYPE_OBJECT, TYPE_OBJECT.into());

        Self {
            map_sym_type_id,
            map_type_id_sym,
            types,
        }
    }
}

impl CtxType {
    pub fn get_type_id(&self, sym: &str) -> Option<&usize> {
        self.map_sym_type_id.get(sym)
    }

    pub fn get_sym(&self, type_id: &usize) -> Option<&String> {
        self.map_type_id_sym.get(type_id)
    }

    pub fn get_type(&self, type_id: usize) -> Option<&LSymType> {
        self.types.get(type_id)
    }

    pub fn get_type_from_sym(&self, sym: &str) -> Option<&LSymType> {
        match self.get_type_id(sym) {
            None => None,
            Some(type_id) => self.get_type(*type_id),
        }
    }

    pub fn bind_sym_type(&mut self, sym: &str, type_id: usize) {
        self.map_sym_type_id.insert(sym.to_string(), type_id);
        self.map_type_id_sym.insert(type_id, sym.to_string());
    }

    pub fn add_type(&mut self, sym_type: LSymType) -> usize {
        self.types.push(sym_type);
        self.types.len() - 1
    }
}

impl GetModule for CtxType {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_TYPE,
        };

        module.add_fn_prelude(IS_STATE_FUNCTION, is_state_function);
        module.add_fn_prelude(IS_OBJECT, is_object);
        module.add_fn_prelude(IS_TYPE, is_type);
        module.add_fn_prelude(GET_TYPE, get_type);
        module.add_mut_fn_prelude(TYPE_OF, type_of);
        module.add_mut_fn_prelude(SUB_TYPE, sub_type);
        module.add_mut_fn_prelude(NEW_STATE_FUNCTION, new_state_function);
        module.add_mut_fn_prelude(NEW_OBJECT, new_object);

        module
    }
}

/*
DOCUMENTATION
 */
//TODO: write doc mod type

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

impl Documentation for CtxType {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_TYPE, DOC_MOD_TYPE, None),
            LHelp::new(IS_STATE_FUNCTION, DOC_IS_STATE_FUNCTION, None),
            LHelp::new(IS_OBJECT, DOC_IS_OBJECT, None),
            LHelp::new(IS_TYPE, DOC_IS_TYPE, None),
            LHelp::new(GET_TYPE, DOC_GET_TYPE, None),
            LHelp::new(TYPE_OF, DOC_TYPE_OF, None),
            LHelp::new(SUB_TYPE, DOC_SUB_TYPE, Some(DOC_SUB_TYPE_VERBOSE)),
            LHelp::new(
                NEW_STATE_FUNCTION,
                DOC_NEW_STATE_FUNCTION,
                Some(DOC_NEW_STATE_FUNCTION_VERBOSE),
            ),
            LHelp::new(NEW_OBJECT, DOC_NEW_OBJECT, Some(DOC_NEW_OBJECT_VERBOSE)),
        ]
    }
}

/*
FUNCTIONS
 */

pub fn is_type(args: &[LValue], _: &LEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match &args[0] {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => Err(SpecialError(IS_TYPE, "symbol has no type".to_string())),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(IS_TYPE, lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(IS_TYPE, args.into(), i, 1..1)),
    }
}

pub fn is_object(args: &[LValue], _: &LEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(IS_OBJECT, lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(IS_OBJECT,args.into(), i, 1..1)),
    }
}

pub fn is_state_function(args: &[LValue], _: &LEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::True),
                    _ => Ok(LValue::Nil),
                },
            },
            lv => Err(WrongType(IS_STATE_FUNCTION, lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(IS_STATE_FUNCTION, args.into(), i, 1..1)),
    }
}

pub fn type_of(args: &[LValue], _: &LEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(TYPE_OF, args.into(), args.len(), 2..2));
    }

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
                    NameTypeLValue::Other("type_id(usize)".to_string()),
                )),
            }
        }
        lv => Err(WrongType(TYPE_OF, lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    }
}

pub fn sub_type(args: &[LValue], _: &LEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SUB_TYPE, args.into(), args.len(), 1..1));
    }
    let expected_type = NameTypeLValue::Other("TYPE".to_string());
    let parent_type: usize = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => return Err(SpecialError(SUB_TYPE, format!("{} has no type annotations", s))),
            Some(lst) => match lst {
                LSymType::Type(_) => *ctx.get_type_id(s).unwrap(),
                lst => return Err(WrongType(SUB_TYPE,lst.into(), lst.into(), expected_type)),
            },
        },
        lv => return Err(WrongType(SUB_TYPE, lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    };

    let type_id = ctx.add_type(LSymType::Type(Some(parent_type)));
    Ok(type_id.into())
}

pub fn new_state_function(
    args: &[LValue],
    env: &LEnv,
    ctx: &mut CtxType,
) -> Result<LValue, LError> {
    let mut t_params: Vec<String> = Vec::new();
    let mut t_value: String = String::from(TYPE_OBJECT);
    let expected_type = NameTypeLValue::Other("TYPE".to_string());
    for (i, arg) in args.iter().enumerate() {
        match arg {
            LValue::Symbol(s) => {
                if is_type(&args[i..i + 1], env, ctx)?.try_into()? {
                    if i == args.len() - 1 {
                        t_value = s.clone();
                    } else {
                        t_params.push(s.clone())
                    }
                } else {
                    return match ctx.get_type_from_sym(s) {
                        None => Err(WrongType(NEW_STATE_FUNCTION, arg.clone(), arg.into(), expected_type)),
                        Some(lst) => Err(WrongType(NEW_STATE_FUNCTION, arg.clone(), lst.into(), expected_type)),
                    };
                }
            }
            lv => return Err(WrongType(NEW_STATE_FUNCTION, lv.clone(), lv.into(), expected_type)),
        }
    }

    let type_id = ctx.add_type(LSymType::StateFunction(LStateFunction {
        t_params,
        t_value,
    }));
    Ok(type_id.into())
}

pub fn new_object(args: &[LValue], _: &LEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(NEW_OBJECT, args.into(), args.len(), 1..1));
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
                Some(u) => *u,
            };
            ctx.get_type(type_id)
        }
        LValue::Number(LNumber::Usize(u)) => {
            type_id = *u;
            ctx.get_type(type_id)
        }
        lv => {
            return Err(NotInListOfExpectedTypes(
                NEW_OBJECT,
                lv.clone(),
                lv.into(),
                vec![NameTypeLValue::Symbol, NameTypeLValue::Usize],
            ))
        }
    };

    match option_type {
        None => Err(WrongType(
            NEW_OBJECT,
            (&args[0]).clone(),
            (&args[0]).into(),
            NameTypeLValue::Symbol,
        )),
        Some(lst) => match lst {
            LSymType::Type(_) => {
                let type_id = ctx.add_type(LSymType::Object(type_id));
                Ok(type_id.into())
            }
            lst => Err(WrongType(
                NEW_OBJECT,
                lst.into(),
                lst.into(),
                NameTypeLValue::Other("type".to_string()),
            )),
        },
    }
}

pub fn get_type(args: &[LValue], _: &LEnv, ctx: &CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(GET_TYPE,args.into(), args.len(), 1..1));
    }
    let type_as_string = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => return Err(LError::SpecialError(GET_TYPE, format!("{} has no type", s))),
            Some(lst) => match lst {
                LSymType::Object(u) => ctx.get_sym(u).unwrap().to_string(),
                LSymType::Type(parent_type) => match parent_type {
                    None => "root type".to_string(),
                    Some(u) => format!("subtype of {}", ctx.get_sym(u).unwrap().to_string()),
                },
                lst => lst.to_string(),
            },
        },
        lv => NameTypeLValue::from(lv).to_string(),
    };

    Ok(type_as_string.into())
}
