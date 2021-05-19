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
pub const NIL: &str = "nil";
pub const FALSE: &str = "FALSE";
pub const NOT: &str = "not";
pub const NOT_SHORT: &str = "!";

//MACRO
///Problem during expansion
pub const MACRO_AND: &str = "(defmacro and (lambda x \
                                               (quasiquote (if (none? (unquote x))\
                                                    true\
                                                    (if (car (unquote x))\
                                                        (and (cdr (unquote x)))\
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
pub const LAMBDA_OR: &str = " (define or (lambda x \
                                               (if (none? x)\
                                                    true\
                                                    (if (car x)\
                                                        true\
                                                        (and (cdr x))))))";
//Documentation

//TODO: Write the doc for the basic functions
pub const DOC_CAR: &str = "";
pub const DOC_CDR: &str = "";
pub const DOC_APPEND: &str = "";
pub const DOC_MEMBER: &str = "";
pub const DOC_LAST: &str = "";
pub const DOC_EMPTY: &str = "";
pub const DOC_LEN: &str = "";
pub const DOC_REVERSE: &str = "";
pub const DOC_LIST: &str = "";
pub const DOC_CONS: &str = "";
pub const DOC_MAP: &str = "";
pub const DOC_GET: &str = "";
pub const DOC_GET_MAP: &str = "";
pub const DOC_SET_MAP: &str = "";

pub const DOC_DEFINE: &str = "";
pub const DOC_DEF_MACRO: &str = "";
pub const DOC_LAMBDA: &str = "";
pub const DOC_IF: &str = "";
pub const DOC_QUOTE: &str = "";
pub const DOC_QUASI_QUOTE: &str = "";
pub const DOC_UNQUOTE: &str = "";
pub const DOC_SET: &str = "";
pub const DOC_BEGIN: &str = "";
