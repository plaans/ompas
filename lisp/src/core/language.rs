pub const TYPE_LIST: &str = "tlist";
pub const MAP: &str = "map";
pub const INT: &str = "int";
pub const BOOL: &str = "bool";
pub const FLOAT: &str = "float";
pub const NUMBER: &str = "number";
pub const SYMBOL: &str = "symbol";
pub const STRING: &str = "string";
pub const CHARACTER: &str = "character";
pub const FUTURE: &str = "future";
pub const TRUE: &str = "true";
pub const NIL: &str = "nil";
pub const ERR: &str = "err";
pub const SEXPR: &str = "sexpr";
pub const FN: &str = "fn";
pub const OBJECT: &str = "object";
pub const ATOM: &str = "atom";
pub const CORE_OPERATOR: &str = "core-operator";
pub const MUT_FN: &str = "mut-fn";
pub const USIZE: &str = "USIZE";
pub const ASYNC_FN: &str = "async-fn";
pub const ASYNC_MUT_FN: &str = "async-mut-fn";
//Core language

//Boolean

pub const FALSE: &str = "false";

pub fn get_symbol_types() -> Vec<&'static str> {
    vec![
        TYPE_LIST,
        MAP,
        INT,
        BOOL,
        FLOAT,
        NUMBER,
        SYMBOL,
        STRING,
        CHARACTER,
        FUTURE,
        TRUE,
        NIL,
        ERR,
        SEXPR,
        FN,
        OBJECT,
        ATOM,
        CORE_OPERATOR,
        MUT_FN,
        USIZE,
        ASYNC_FN,
        ASYNC_MUT_FN,
    ]
}
