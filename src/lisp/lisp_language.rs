//Mathematical functions
pub const ADD: &str = "+";
pub const SUB: &str = "-";
pub const MUL: &str = "*";
pub const DIV: &str = "/";

//Comparison
pub const GT: &str = ">";
pub const LT: &str = "<";
pub const GE: &str = ">=";
pub const LE: &str = "<=";
pub const EQ: &str = "=";

//Verification
pub const IS_NONE: &str = "none?";
pub const IS_NUMBER: &str = "number?";
pub const IS_BOOL: &str = "bool?";
pub const IS_FN: &str = "fn?";
pub const IS_TYPE: &str = "type?";
pub const IS_STATE_VARIABLE: &str = "sv?";
pub const IS_STATE_FUNCTION: &str = "sf?";
pub const IS_FACTBASE: &str = "fb?";
pub const IS_OBJECT: &str = "obj?";
pub const IS_PAIR: &str = "pair?";
pub const IS_LIST: &str = "list?";
pub const IS_MAP: &str = "map?";
pub const IS_LAMBDA: &str = "lambda?";
pub const IS_QUOTE: &str = "quote?";

//FactBase language
pub const OBJECT: &str = "obj";
pub const STATE_FUNCTION: &str = "sf";
pub const STATE_VARIABLE: &str = "sv";
pub const FACTBASE: &str = "fb";
pub const TYPE: &str = "type";
pub const STATE: &str = "state";

pub const MAP: &str = "map";
pub const PAIR: &str = "pair";
pub const TYPEOF: &str = "typeof";
pub const SUBTYPE: &str = "subtype";
pub const GET_TYPE: &str = "get-type";

/*
LIST FUNCTIONS
 */
pub const CAR: &str = "car";
pub const CDR: &str = "cdr";
pub const APPEND: &str = "append";
pub const LAST: &str = "last";
pub const MEMBER: &str = "member";
pub const REVERSE:&str = "reverse";
pub const LIST: &str = "list";
pub const CONS: &str = "cons";

pub const AND : &str = "and";
pub const OR: &str = "or";
pub const NOT: &str = "not";

//basic types
pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_OBJECT: &str = "object";
pub const TYPE_BOOL: &str = "boolean";
pub const TYPE_ROOT: &str = "root";

//Other
pub const SET_FACTBASE: &str = "setfb";
pub const READ: &str = "read";
pub const WRITE: &str = "write";
pub const GET: &str = "get";
pub const PRINT: &str = "print";

//Core language
pub const DEFINE: &str = "define";
pub const DEF_MACRO: &str = "defmacro";
pub const LAMBDA: &str = "lambda";
pub const IF: &str = "if";
pub const QUOTE: &str = "quote";
pub const QUASI_QUOTE: &str = "quasiquote";
pub const UNQUOTE: &str= "unquote";
pub const SET: &str = "set!";
pub const BEGIN: &str = "begin";



//Boolean
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";

pub const PI: &str = "pi";
