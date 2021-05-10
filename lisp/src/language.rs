/*
LIST FUNCTIONS
 */
pub const CAR: &str = "car";
pub const CDR: &str = "cdr";
pub const APPEND: &str = "append";
pub const LAST: &str = "last";
pub const EMPTY: &str = "empty";
pub const LEN: &str = "len";
pub const MEMBER: &str = "member";
pub const REVERSE: &str = "reverse";
pub const LIST: &str = "list";
pub const CONS: &str = "cons";

//Other
pub const GET: &str = "get";
pub const GET_MAP: &str = "get-map";
pub const SET_MAP: &str = "set-map";
pub const MAP: &str = "map";
pub const PAIR: &str = "pair";

//Core language
pub const DEFINE: &str = "define";
pub const DEF_MACRO: &str = "defmacro";
pub const LAMBDA: &str = "lambda";
pub const IF: &str = "if";
pub const QUOTE: &str = "quote";
pub const QUASI_QUOTE: &str = "quasiquote";
pub const UNQUOTE: &str = "unquote";
pub const SET: &str = "set!";
pub const BEGIN: &str = "begin";

//Boolean
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const NONE: &str = "none";
pub const NOT: &str = "not";
pub const NOT_SHORT: &str = "!";

//MACRO
pub const MACRO_AND: &str = "(define and (lambda l (quasiquote
                                                (if (< (len (unquote l)) 1 )\
                                                    true\
                                                    (if (car (unquote l))\
                                                        (and (cdr (unquote l)))\
                                                        false)))))";

pub const MACRO_AND2: &str = "(defmacro and2 (lambda (a b) \
                                                (quasiquote (if (unquote a) \
                                                                (unquote b) \
                                                                false))))";
pub const MACRO_OR2: &str = "(defmacro or2  (lambda (a b) \
                                                (quasiquote (if (unquote a) \
                                                                true \
                                                                (unquote b)))))";

pub const MACRO_NEQ: &str = "(defmacro neq (lambda (a b)\
                                            (! (= a b))))";

pub const MACRO_NEQ_SHORT: &str = "(defmacro != (lambda (a b) \
                                                (neq a b )))";

pub const LAMBDA_AND: &str = " (define and (lambda x \
                                               (if (none? x)\
                                                    true\
                                                    (if (car x)\
                                                        (and (cdr x))\
                                                        false))))";
//Documentation
