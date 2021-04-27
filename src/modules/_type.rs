use crate::core::lisp_as_literal::AsLiteral;
use crate::core::r#struct::LError::*;
use crate::core::r#struct::*;
use crate::core::RefLEnv;
use aries_utils::input::Sym;
use im::HashMap;
use std::fmt::{Debug, Display, Formatter};

//pub const TYPE: &str = "type";
//pub const STATE: &str = "state";
//pub const IS_PAIR: &str = "pair?";

//Verification
pub const IS_NONE: &str = "none?";
pub const IS_NUMBER: &str = "number?";
pub const IS_BOOL: &str = "bool?";
pub const IS_SYMBOL: &str = "sym?";
pub const IS_FN: &str = "fn?";
pub const IS_MUT_FN: &str = "mut-fn?";
pub const IS_TYPE: &str = "type?";
pub const IS_STATE_FUNCTION: &str = "sf?";
pub const IS_OBJECT: &str = "obj?";
pub const IS_LIST: &str = "list?";
pub const IS_MAP: &str = "map?";
pub const IS_LAMBDA: &str = "lambda?";
pub const IS_QUOTE: &str = "quote?";

//FactBase language
pub const STATE_FUNCTION: &str = "state-function";

//basic types
pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_USIZE: &str = "usize";
pub const TYPE_OBJECT: &str = "object";
pub const TYPE_BOOL: &str = "boolean";

pub const INDEX_TYPE_INT: usize = 0;
pub const INDEX_TYPE_FLOAT: usize = 1;
pub const INDEX_TYPE_USIZE: usize = 2;
pub const INDEX_TYPE_OBJECT: usize = 3;
pub const INDEX_TYPE_BOOL: usize = 4;

pub const TYPE_OF: &str = "type-of";
pub const SUB_TYPE: &str = "sub-type";
pub const GET_TYPE: &str = "get-type";
pub const NEW_STATE_FUNCTION: &str = "new-sf";
pub const NEW_OBJECT: &str = "new-obj";

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
                lst.into(),
                NameTypeLValue::Other(STATE_FUNCTION.to_string()),
            )),
        }
    }
}

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
}

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
    Symbol(Sym),
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

impl From<&Sym> for LType {
    fn from(s: &Sym) -> Self {
        s.as_str().into()
    }
}

impl From<Sym> for LType {
    fn from(s: Sym) -> Self {
        (&s).into()
    }
}

impl From<&LType> for Sym {
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

impl From<LSymType> for NameTypeLValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl From<&LSymType> for LValue {
    fn from(lst: &LSymType) -> Self {
        LValue::String(lst.to_string())
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

impl From<LType> for Sym {
    fn from(lt: LType) -> Self {
        (&lt).into()
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
    pub t_params: Vec<Sym>,
    pub t_value: Sym,
}

impl Display for LStateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut sr = String::new();
        sr.push('(');
        for (i, t_param) in self.t_params.iter().enumerate() {
            sr.push_str(format!("{}", t_param).as_str());
            if i > 0 {
                sr.push(',');
            }
        }
        sr.push_str(format!(") = {}", self.t_value).as_str());
        write!(f, "{}", sr)
    }
}

impl AsLiteral for LStateFunction {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", STATE_FUNCTION).as_str());
        for t_param in &self.t_params {
            result.push_str(format!("{} ", t_param.to_string()).as_str());
        }
        result.push_str(format!("{})\n", self.t_value.to_string()).as_str());
        result
    }
}

#[derive(Debug)]
pub struct CtxType {
    map_sym_type_id: HashMap<Sym, usize>,
    map_type_id_sym: HashMap<usize, Sym>,
    types: Vec<LSymType>,
}

//TODO: IMPROVE GET-TYPE and DEFAULT
impl Default for CtxType {
    fn default() -> Self {
        let types = vec![LSymType::Type(None); 5];

        let mut map_sym_type_id: HashMap<Sym, usize> = Default::default();
        map_sym_type_id.insert(TYPE_INT.into(), INDEX_TYPE_INT);
        map_sym_type_id.insert(TYPE_FLOAT.into(), INDEX_TYPE_FLOAT);
        map_sym_type_id.insert(TYPE_USIZE.into(), INDEX_TYPE_USIZE);
        map_sym_type_id.insert(TYPE_BOOL.into(), INDEX_TYPE_BOOL);
        map_sym_type_id.insert(TYPE_OBJECT.into(), INDEX_TYPE_OBJECT);

        let mut map_type_id_sym: HashMap<usize, Sym> = Default::default();
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
    pub fn get_type_id(&self, sym: &Sym) -> Option<&usize> {
        self.map_sym_type_id.get(sym)
    }

    pub fn get_sym(&self, type_id: &usize) -> Option<&Sym> {
        self.map_type_id_sym.get(type_id)
    }

    pub fn get_type(&self, type_id: usize) -> Option<&LSymType> {
        self.types.get(type_id)
    }

    pub fn get_type_from_sym(&self, sym: &Sym) -> Option<&LSymType> {
        match self.get_type_id(sym) {
            None => None,
            Some(type_id) => self.get_type(*type_id),
        }
    }

    pub fn bind_sym_type(&mut self, sym: &Sym, type_id: usize) {
        self.map_sym_type_id.insert(sym.clone(), type_id);
        self.map_type_id_sym.insert(type_id, sym.clone());
    }

    pub fn add_type(&mut self, sym_type: LSymType) -> usize {
        self.types.push(sym_type);
        self.types.len() - 1
    }
}

impl AsModule for CtxType {
    fn get_module() -> Module {
        let mut prelude = vec![];

        //Type verification
        prelude.push((
            IS_NONE.into(),
            LValue::Fn(LFn::new(Box::new(is_none), IS_NONE.to_string())),
        ));
        prelude.push((
            IS_NUMBER.into(),
            LValue::Fn(LFn::new(Box::new(is_number), IS_NUMBER.to_string())),
        ));
        prelude.push((
            IS_BOOL.into(),
            LValue::Fn(LFn::new(Box::new(is_bool), IS_BOOL.to_string())),
        ));
        prelude.push((
            IS_SYMBOL.into(),
            LValue::Fn(LFn::new(Box::new(is_symbol), IS_SYMBOL.to_string())),
        ));
        prelude.push((
            IS_FN.into(),
            LValue::Fn(LFn::new(Box::new(is_fn), IS_FN.to_string())),
        ));
        prelude.push((
            IS_MUT_FN.into(),
            LValue::Fn(LFn::new(Box::new(is_mut_fn), IS_MUT_FN.to_string())),
        ));
        prelude.push((
            IS_STATE_FUNCTION.into(),
            LValue::Fn(LFn::new(
                Box::new(is_state_function),
                IS_STATE_FUNCTION.to_string(),
            )),
        ));
        prelude.push((
            IS_OBJECT.into(),
            LValue::Fn(LFn::new(Box::new(is_object), IS_OBJECT.to_string())),
        ));
        prelude.push((
            IS_TYPE.into(),
            LValue::Fn(LFn::new(Box::new(is_type), IS_TYPE.to_string())),
        ));
        prelude.push((
            IS_MAP.into(),
            LValue::Fn(LFn::new(Box::new(is_map), IS_MAP.to_string())),
        ));
        prelude.push((
            IS_LIST.into(),
            LValue::Fn(LFn::new(Box::new(is_list), IS_LIST.to_string())),
        ));
        prelude.push((
            IS_LAMBDA.into(),
            LValue::Fn(LFn::new(Box::new(is_lambda), IS_LAMBDA.to_string())),
        ));
        prelude.push((
            IS_QUOTE.into(),
            LValue::Fn(LFn::new(Box::new(is_quote), IS_QUOTE.to_string())),
        ));

        prelude.push((
            GET_TYPE.into(),
            LValue::Fn(LFn::new(Box::new(get_type), GET_TYPE.to_string())),
        ));

        prelude.push((
            TYPE_OF.into(),
            LValue::MutFn(LMutFn::new(Box::new(type_of), TYPE_OF.to_string())),
        ));

        prelude.push((
            SUB_TYPE.into(),
            LValue::MutFn(LMutFn::new(Box::new(subtype), SUB_TYPE.to_string())),
        ));

        prelude.push((
            NEW_STATE_FUNCTION.into(),
            LValue::MutFn(LMutFn::new(
                Box::new(new_state_function),
                NEW_STATE_FUNCTION.to_string(),
            )),
        ));

        prelude.push((
            NEW_OBJECT.into(),
            LValue::MutFn(LMutFn::new(
                Box::new(new_object),
                NEW_STATE_FUNCTION.to_string(),
            )),
        ));

        Module {
            ctx: Box::new(CtxType::default()),
            prelude,
        }
    }
}

/*
FUNCTIONS
 */

//Type verification
pub fn is_none(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::None,
        )),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_number(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Number,
        )),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_bool(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Bool,
        )),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_fn(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Fn,
        )),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_mut_fn(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::MutFn,
        )),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_type(args: &[LValue], _: &RefLEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match &args[0] {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => Err(SpecialError("symbol has no type".to_string())),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_symbol(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_object(args: &[LValue], _: &RefLEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_state_function(args: &[LValue], _: &RefLEnv, ctx: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_map(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}
pub fn is_list(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::List(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_lambda(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_quote(args: &[LValue], _: &RefLEnv, _: &CtxType) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Quote(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn type_of(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Symbol(s) => {
            let lv = &args[1];
            match lv {
                LValue::Number(LNumber::Usize(u)) => {
                    ctx.bind_sym_type(s, *u);
                    Ok(LValue::None)
                }
                lv => Err(WrongType(
                    lv.clone(),
                    lv.into(),
                    NameTypeLValue::Other("type_id(usize)".to_string()),
                )),
            }
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    }
}

pub fn subtype(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    let expected_type = NameTypeLValue::Other("TYPE".to_string());
    let parent_type: usize = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => return Err(SpecialError(format!("{} has no type annotations", s))),
            Some(lst) => match lst {
                LSymType::Type(_) => *ctx.get_type_id(s).unwrap(),
                lst => return Err(WrongType(lst.into(), lst.into(), expected_type)),
            },
        },
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    };

    let type_id = ctx.add_type(LSymType::Type(Some(parent_type)));
    Ok(type_id.into())
}

pub fn new_state_function(
    args: &[LValue],
    env: &mut RefLEnv,
    ctx: &mut CtxType,
) -> Result<LValue, LError> {
    let mut t_params: Vec<Sym> = Vec::new();
    let mut t_value: Sym = Sym::from(TYPE_OBJECT);
    let expected_type = NameTypeLValue::Other("TYPE".to_string());
    for (i, arg) in args.iter().enumerate() {
        match arg {
            LValue::Symbol(s) => {
                if is_type(&args[i..i + 1], env, ctx)?.as_bool()? {
                    if i == args.len() - 1 {
                        t_value = s.clone();
                    } else {
                        t_params.push(s.clone())
                    }
                } else {
                    return match ctx.get_type_from_sym(s) {
                        None => Err(WrongType(arg.clone(), arg.into(), expected_type)),
                        Some(lst) => Err(WrongType(arg.clone(), lst.into(), expected_type)),
                    };
                }
            }
            lv => return Err(WrongType(lv.clone(), lv.into(), expected_type)),
        }
    }

    let type_id = ctx.add_type(LSymType::StateFunction(LStateFunction {
        t_params,
        t_value,
    }));
    Ok(type_id.into())
}

pub fn new_object(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    let type_id;
    let option_type = match &args[0] {
        LValue::Symbol(s) => {
            type_id = match ctx.get_type_id(s) {
                None => {
                    return Err(SpecialError(
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
                lv.clone(),
                lv.into(),
                vec![NameTypeLValue::Symbol, NameTypeLValue::Usize],
            ))
        }
    };

    match option_type {
        None => Err(WrongType(
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
                lst.into(),
                lst.into(),
                NameTypeLValue::Other("type".to_string()),
            )),
        },
    }
}

pub fn get_type(args: &[LValue], _: &RefLEnv, ctx: &CtxType) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    let type_as_string = match &args[0] {
        LValue::Symbol(s) => match ctx.get_type_from_sym(s) {
            None => return Err(LError::SpecialError(format!("{} has no type", s))),
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
