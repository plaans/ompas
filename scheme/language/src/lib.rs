pub const LIST: &str = "list";
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

pub const DEFINE: &str = "define";
pub const DEF_MACRO: &str = "defmacro";
pub const LAMBDA: &str = "lambda";
pub const IF: &str = "if";
pub const QUOTE: &str = "quote";
pub const QUASI_QUOTE: &str = "quasiquote";
pub const UNQUOTE: &str = "unquote";
pub const QUOTE_CHAR: char = '\'';
pub const UNQUOTE_CHAR: char = ',';
pub const QUASI_QUOTE_CHAR: char = '`';
pub const BEGIN: &str = "begin";
pub const ASYNC: &str = "async";
pub const AWAIT: &str = "await";
pub const RACE: &str = "race";
pub const EVAL: &str = "eval";
pub const PARSE: &str = "parse";
pub const EXPAND: &str = "expand";
pub const DO: &str = "do";

pub const DOC_DEFINE: &str = "Defines a new entry in the environment/";
pub const DOC_DEF_MACRO: &str = "Define a new macro, can only be done at the top level";
pub const DOC_LAMBDA: &str = "Define a new lambda function";
pub const DOC_LAMBDA_VEBROSE: &str = "Example:\n\
                                \t>> (define square (lambda (x) (* x x)))\n\
                                \t>> (square 10)\n\
                                \tLI>> 100";
pub const DOC_IF: &str = "Condition block";
pub const DOC_QUOTE: &str = "Prevent the LValue to be evaluated";
pub const DOC_QUASI_QUOTE: &str = "Begins a context mixing quoting and unquoting";
pub const DOC_UNQUOTE: &str = "Evaluated an expression present inside a quasi-quote statement";
pub const DOC_BEGIN: &str = "Evaluated a list LValue and returns the last one.";
pub const DOC_BEGIN_VERBOSE: &str = "Example: \n\
                                 \t>>(begin 10 (* 3 3))\n\
                                 \tLI>> 9";

pub const DOC_ASYNC: &str = "Evaluate asynchronously a LValue. Returns the pid(usize) of the task";
pub const DOC_AWAIT: &str = "Await on a pid to get the result of the evaluation.";
pub const DOC_EVAL: &str = "Eval a LValue.";

pub const ENV_GET_KEYS: &str = "get_keys";
//return a list of keys of the environment
pub const ENV_GET_MACROS: &str = "get_macros";
pub const ENV_GET_MACRO: &str = "get_macro";
pub const ENV_GET_LIST_MODULES: &str = "get_list_modules";

pub const HELP: &str = "help";
pub const DOC_HELP: &str =
    "Give a list of all the available functions added by the modules and available in the core.";
pub const DOC_HELP_VERBOSE: &str = "takes 0..1 arguments:\
                            -no argument: give the list of all the functions\n\
                            -1 argument: give the documentation of the function.";
pub const ARBITRARY: &str = "arbitrary";
pub const RAND_ELEMENT: &str = "rand-element";
pub const ENUMERATE: &str = "enumerate";
pub const CONTAINS: &str = "contains";
pub const SUB_LIST: &str = "sublist";
pub const QUOTE_LIST: &str = "quote-list";
pub const TRANSFORM_IN_SINGLETON_LIST: &str = "transform-in-singleton-list";

pub const LET: &str = "let";
pub const LET_STAR: &str = "let*";
pub const COND: &str = "cond";
pub const TEST_MACRO: &str = "test-macro";
//Not yet implemented
pub const FN_MAP: &str = "map";
pub const APPLY: &str = "APPLY";
pub const ZIP: &str = "zip";
pub const UNZIP: &str = "unzip";
pub const COMBINE: &str = "combine";
pub const FIRST: &str = "first";
pub const SECOND: &str = "second";
pub const THIRD: &str = "third";
pub const REST: &str = "rest";
pub const CAR: &str = "car";
pub const CDR: &str = "cdr";
pub const APPEND: &str = "append";
pub const LAST: &str = "last";
pub const EMPTY: &str = "empty";
pub const LEN: &str = "len";
pub const MEMBER: &str = "member";
pub const REVERSE: &str = "reverse";
pub const GET_LIST: &str = "get-list";
pub const SET_LIST: &str = "set-list";
pub const CONS: &str = "cons";
pub const INTERSECTION: &str = "intersection";

pub const DOC_FIRST: &str = "Return the first element of a list or nil.";
pub const DOC_SECOND: &str = "Return the second element of a list or nil.";
pub const DOC_THIRD: &str = "Return the third element of a list or nil.";
pub const DOC_REST: &str = "Same as cdr";
pub const DOC_CAR: &str =
    "Takes a list of at least one element, and return the first element. Nil otherwise.";
pub const DOC_CDR: &str =
    "Takes a list of at least one element, and return a list without the first element.";
pub const DOC_APPEND: &str = "Takes two list and return a list merging both.";
pub const DOC_MEMBER: &str = "Takes two arguments of which the second must be a list. \
if the first argument is a member of the second argument,\
and then it returns the remainder of the list beginning with the first argument.";
pub const DOC_LAST: &str =
    "Takes a list of at least one element and return the last element. Nil otherwise";
pub const DOC_EMPTY: &str = "Return true if the a list or map is empty.";
pub const DOC_LEN: &str = "Return the len of a list or a map";
pub const DOC_REVERSE: &str = "Takes a list and return a list with all elements reversed in order";
pub const DOC_LIST: &str = "Return a list of the LValues given is argument";
pub const DOC_GET_LIST: &str = "todo!";
pub const DOC_SET_LIST: &str = "todo!";
pub const DOC_CONS: &str = "Takes two objects and merges into a list.";

pub const SET: &str = "set";
pub const GET: &str = "get";

//Mathematical functions
pub const ADD: &str = "+";
pub const SUB: &str = "-";
pub const MUL: &str = "*";
pub const DIV: &str = "/";

//Comparison
pub const GT: &str = ">";
pub const LT: &str = "<";
pub const GEQ: &str = ">=";
pub const LEQ: &str = "<=";
pub const EQ: &str = "=";
pub const NEQ: &str = "!=";

pub const DOC_EQ: &str =
    "Takes 2 arguments. Return true if two arguments are equal. False otherwise.";
pub const DOC_ADD: &str = "Takes 2+ arguments. Return the addition.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
pub const DOC_SUB: &str = "Takes 2 arguments. Return the substraction of the first by the second.\
return an error if inputs are not numbers or there is wrong numbers of arguments";
pub const DOC_MUL: &str = "Takes 2+ arguments. Return the multiplication.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
pub const DOC_DIV: &str = "Takes 2 arguments. Return the division of the first by the second.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
pub const DOC_GT: &str = "Takes 2 arguments. Return *true* if the first is greater than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
pub const DOC_GEQ: &str = "Takes 2 arguments. Return *true* if the first is greater or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
pub const DOC_LT: &str = "Takes 2 arguments. Return *true* if the first is less than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
pub const DOC_LEQ: &str = "Takes 2 arguments. Return *true* if the first is less or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

pub const NOT: &str = "not";
pub const NOT_SHORT: &str = "!";
pub const IS_ERR: &str = "err?";
pub const DOC_ERR: &str = "Return an LValue::Err(LValue)";
pub const DOC_IS_ERR: &str =
    "Return true if the argument is an lvalue of the form (err <expr>), false otherwise.";

pub const CHECK: &str = "check";
pub const DOC_CHECK: &str = "Return an LValue::Err if the LValue if false";

pub const GET_MAP: &str = "get-map";
pub const SET_MAP: &str = "set-map";
pub const REMOVE_MAP: &str = "remove-map";
pub const REMOVE_KEY_VALUE_MAP: &str = "remove-key-value-map";
pub const UNION_MAP: &str = "union-map";

pub const DOC_MAP: &str = "Return a map from from a list of pairs.";
pub const DOC_MAP_VERBOSE: &str = "Example: (map (quote ((ten . 10) (twenty . 20))))";
pub const DOC_GET: &str = "Takes a key as argument and return the binding if defined in the environment. Return the key otherwise.";
pub const DOC_GET_MAP: &str = "Takes a map and a key as argument, and return the value associated.";
pub const DOC_GET_MAP_VERBOSE: &str = "Example: Here is an example in the repl\n\
                                    \t>> (define m (map (quote ((ten . 10) (twenty . 20)))))\n\
                                    \t>> (get-map m ten)\n\
                                    \tLI>> 10";
pub const DOC_SET_MAP: &str = "Takes a map and and a list of pairs (key . value) to set in the map. Return a new map with the new bindings";
pub const DOC_SET_MAP_VERBOSE: &str = "Example: Here is an example in the repl\n\
                                    \t>> (define m (map (quote ((ten . 10) (twenty . 20)))))\n\
                                    \t>> (get m)\n\
                                    \tLI>> ten: 10 \n\
                                    \ttwenty: 20 \n\
                                    \t>> (define m (set-map m (three . 3 )))\n\
                                    \tLI>> (get m)\n\
                                    \tLI>> ten: 10 \n\
                                    \tthree: 3\n\
                                    \ttwenty: 20";

pub const IS_NUMBER: &str = "number?";
pub const IS_FLOAT: &str = "float?";
pub const IS_INT: &str = "int?";
pub const IS_BOOL: &str = "bool?";
pub const IS_SYMBOL: &str = "symbol?";
pub const IS_STRING: &str = "string?";
pub const IS_FN: &str = "fn?";
pub const IS_MUT_FN: &str = "mut-fn?";
pub const IS_LIST: &str = "list?";
pub const IS_MAP: &str = "map?";
pub const IS_LAMBDA: &str = "lambda?";
pub const IS_QUOTE: &str = "quote?";
pub const IS_PAIR: &str = "pair?";
pub const IS_EQUAL: &str = "equal?";

pub const IS_NIL: &str = "null?";

pub const DOC_IS_NIL: &str = "Return true if symbol is LValue::Nil or empty list.";
pub const DOC_IS_NUMBER: &str = "Return true if symbol is LValue::Number";
pub const DOC_IS_BOOL: &str = "Return true if symbol is LValue::Bool";
pub const DOC_IS_SYMBOL: &str = "Return true if symbol is LValue::Symbol";
pub const DOC_IS_MAP: &str = "Return true if symbol is map";
pub const DOC_IS_LIST: &str = "Return true if symbol is list";
pub const DOC_IS_LAMBDA: &str = "Return true if symbol is lambda";
pub const DOC_IS_QUOTE: &str = "Return true if symbol is quote";
pub const DOC_IS_FN: &str = "Return true if symbol is LValue::Fn";
pub const DOC_IS_MUT_FN: &str = "Return true if symbol is LValue::MutFn";

pub const DOC_IS_PAIR: &str = "Return true if the list is not empty, false otherwise.";
pub const DOC_IS_EQUAL: &str = "Return true if the 2 LValues are equal, false otherwise.";
