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
pub const FALSE: &str = "false";
pub const NOT: &str = "not";
pub const NOT_SHORT: &str = "!";

pub const ENV: &str = "env"; //return a list of keys of the environment

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
pub const DOC_CONS: &str = "Takes two objects and merges into a list.";
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
pub const DOC_SET: &str = "Set the value of an entry of the environment";
pub const DOC_BEGIN: &str = "Evaluated a list LValue and returns the last one.";
pub const DOC_BEGIN_VERBOSE: &str = "Example: \n\
                                     \t>>(begin 10 (* 3 3))\n\
                                     \tLI>> 9";
