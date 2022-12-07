use crate::kind::*;
use crate::primitives::*;

pub const MOD_STD: &str = "std";
pub const DOC_MOD_STD: &str = "Collection of functions of the standard library";

pub mod kind {
    pub const LIST: &str = "list";
    pub const MAP: &str = "map";
    pub const INT: &str = "int";
    pub const BOOL: &str = "bool";
    pub const FLOAT: &str = "float";
    pub const NUMBER: &str = "number";
    pub const SYMBOL: &str = "symbol";
    pub const STRING: &str = "string";
    pub const LAMBDA: &str = "lambda";
    pub const CHARACTER: &str = "character";
    pub const HANDLER: &str = "handle";
    pub const TRUE: &str = "true";
    pub const NIL: &str = "nil";
    pub const ERR: &str = "err";
    pub const SEXPR: &str = "sexpr";
    pub const FN: &str = "fn";
    pub const OBJECT: &str = "object";
    pub const ATOM: &str = "atom";
    pub const PRIMITIVE: &str = "primitive";
    pub const MUT_FN: &str = "mut-fn";
    pub const USIZE: &str = "usize";
    pub const ASYNC_FN: &str = "async-fn";
    pub const ASYNC_MUT_FN: &str = "async-mut-fn";
}
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
        HANDLER,
        TRUE,
        NIL,
        ERR,
        SEXPR,
        FN,
        OBJECT,
        ATOM,
        PRIMITIVE,
        MUT_FN,
        USIZE,
        ASYNC_FN,
        ASYNC_MUT_FN,
    ]
}

pub fn get_primitives() -> Vec<&'static str> {
    vec![
        DEFINE,
        DEF_MACRO,
        FN_LAMBDA,
        IF,
        QUOTE,
        QUASI_QUOTE,
        UNQUOTE,
        BEGIN,
        ASYNC,
        AWAIT,
        RACE,
        ENR,
        EVAL,
        PARSE,
        EXPAND,
        DO,
        INTERRUPT,
        UNINTERRUPTIBLE,
        INTERRUPTIBLE,
        UNINTERRUPTIBLE_SHORT,
        INTERRUPTIBLE_SHORT,
    ]
}

pub mod primitives {
    pub const MOD_PRIMITIVE: &str = "primitive";
    pub const DOC_MOD_PRIMITIVE: &str = "Collection of primitives of the language";

    pub const DEFINE: &str = "define";
    pub const DOC_DEFINE: &str = "Defines a new entry in the environment.";

    pub const DEF_MACRO: &str = "defmacro";
    pub const DOC_DEF_MACRO: &str = "Define a new macro, can only be done at the top level.";

    pub const FN_LAMBDA: &str = "lambda";
    pub const DOC_FN_LAMBDA: &str = "Define a new lambda function";
    pub const DOC_FN_LAMBDA_VERBOSE: &str = "Example:\n\
                                \t>> (define square (lambda (x) (* x x)))\n\
                                \t>> (square 10)\n\
                                \tLI>> 100";

    pub const IF: &str = "if";
    pub const DOC_IF: &str =
        "Conditional evaluation of two expressions in function of a boolean expression.";
    pub const DOC_IF_VERBOSE: &str = "Let us take the expression (if cond a b).\
 If cond == true, then the result of the expression is the result of the evaluation of a, otherwise the result is the evaluation of b.";

    pub const QUOTE: &str = "quote";
    pub const DOC_QUOTE: &str = "Prevent the LValue to be evaluated";

    pub const QUASI_QUOTE: &str = "quasiquote";
    pub const DOC_QUASI_QUOTE: &str = "Begins a context mixing quoting and unquoting";

    pub const UNQUOTE: &str = "unquote";
    pub const DOC_UNQUOTE: &str = "Evaluated an expression present inside a quasi-quote statement";

    pub const QUOTE_CHAR: char = '\'';
    pub const DOC_QUOTE_CHAR: &str = "Short character for quote.";

    pub const QUASI_QUOTE_CHAR: char = '`';
    pub const DOC_QUASI_QUOTE_CHAR: &str = "Short character for quasiquote";

    pub const UNQUOTE_CHAR: char = ',';
    pub const DOC_UNQUOTE_CHAR: &str = "Short character for unquote.";

    pub const BEGIN: &str = "begin";
    pub const DOC_BEGIN: &str = "Evaluate a list of LValue and returns the result of the last one.";
    pub const DOC_BEGIN_VERBOSE: &str = "Example: \n\
                                 \t>>(begin 10 (* 3 3))\n\
                                 \tLI>> 9";

    pub const ASYNC: &str = "async";
    pub const DOC_ASYNC: &str = "Evaluate asynchronously a LValue. Returns a handle that can be either awaited on or interrupted.";
    pub const DOC_ASYNC_VERBOSE: &str = "Example:\n\
\t>>(async (* 3 3))\n\
\tLI>> handle";

    pub const AWAIT: &str = "await";
    pub const DOC_AWAIT: &str =
        "Await on a handle to get the result of the asynchronous evaluation.";
    pub const DOC_AWAIT_VERBOSE: &str = "Example:\n\
\t>>(await (async (* 3 3)))\n\
\tLI>> 9";

    pub const RACE: &str = "race";
    pub const DOC_RACE: &str ="Evaluate two expressions in parallel. Return the result of the first one and interrupt the evaluation of the other one.";

    pub const ENR: &str = "enr";
    pub const DOC_ENR: &str = "Evaluate an expression without evaluating its parameters.";

    pub const EVAL: &str = "eval";
    pub const DOC_EVAL: &str = "Eval an expression.";

    pub const PARSE: &str = "parse";
    pub const DOC_PARSE: &str = "Parse a string.";

    pub const EXPAND: &str = "expand";
    pub const DOC_EXPAND: &str = "Expand an expression and return the expanded expression.";

    pub const DO: &str = "do";
    pub const DOC_DO: &str ="Evaluate a list of expression and stops at the first encountered error, and return the last result otherwise.";

    pub const INTERRUPT: &str = "interrupt";
    pub const DOC_INTERRUPT: &str =
        "Send a interruption signal to a handle, and await the result of the interruption.";

    pub const UNINTERRUPTIBLE: &str = "uninterruptible";
    pub const DOC_UNINTERRUPTIBLE: &str= "Define an expression as uninterruptible. \
    In case a interruption signal is received, the evaluation of the expression will be terminated before propagating the interruption signal.";

    pub const INTERRUPTIBLE: &str = "interruptible";
    pub const DOC_INTERRUPTIBLE: &str = "Define an expression as interruptible.";

    pub const UNINTERRUPTIBLE_SHORT: &str = "u!";
    pub const DOC_UNINTERRUPTIBLE_SHORT: &str = "Character for the primitive uninterruptible.";

    pub const INTERRUPTIBLE_SHORT: &str = "i!";
    pub const DOC_INTERRUPTIBLE_SHORT: &str = "Character for the primitive interruptible.";
}

pub mod env {
    pub const MOD_ENV: &str = "env";
    pub const DOC_MOD_ENV: &str =
        "Collection of environment functions contained in the root module.";

    pub const GET_KEYS: &str = "get_keys";
    pub const DOC_GET_KEYS: &str = "Return all the keys defined in the environment.";

    pub const GET_MACROS: &str = "get_macros";
    pub const DOC_GET_MACROS: &str = "Return the list of macros defined in the environment.";

    pub const GET_MACRO: &str = "get_macro";
    pub const DOC_GET_MACRO: &str = "Return the expression of a given macro";

    pub const GET_CONTEXTS: &str = "get_contexts";
    pub const DOC_GET_CONTEXTS: &str = "Return the list of contexts defined in the environment";

    pub const HELP: &str = "help";
    pub const DOC_HELP: &str =
        "Give a list of all the available functions added by the modules and available in the core.";
    pub const DOC_HELP_VERBOSE: &str = "takes 0..1 arguments:\
                            -no argument: give the list of all the functions\n\
                            -1 argument: give the documentation of the function.";

    pub const GET_PROCESS_HIERARCHY: &str = "get_process_hierarchy";
    pub const DOC_GET_PROCESS_HIERARCHY: &str =
        "Return the list of processes and process topics along their dependencies.";
}

pub mod utils {

    //LANGUAGE
    pub const MOD_UTILS: &str = "utils";
    pub const DOC_MOD_UTILS: &str = "Collection of utility functions.";

    pub const ENUMERATE: &str = "enumerate";
    pub const DOC_ENUMERATE: &str =
        "Return a enumeration of all possible combinations of elements of 1+ lists.";
    pub const DOC_ENUMERATE_VERBOSE: &str =
        "Example: \n(enumerate '(1 2) '(3 4))\n=> ((1 3)(1 4)(2 3)(2 4))";

    pub const RAND_ELEMENT: &str = "rand-element";
    pub const DOC_RAND_ELEMENT: &str = "Return a random element of a list.";
    pub const DOC_RAND_ELEMENT_VERBOSE: &str = "Example: \n(rand-element '(1 2 3 4))\n=> 1";

    pub const CONTAINS: &str = "contains";
    pub const DOC_CONTAINS: &str =
        "Returns true if a LValue is contains into an other (for a map if the key is inside it)";

    pub const SUBLIST: &str = "sublist";
    pub const DOC_SUBLIST: &str = "Returns a sublist of a list.";
    pub const DOC_SUBLIST_VERBOSE: &str = "Example: (sublist '(1 2 3 4 5) 1 3) => (2 3 4)";

    pub const QUOTE_LIST: &str = "quote-list";
    pub const DOC_QUOTE_LIST: &str =
        "Transform a list of elements into a list of quotted elements.";
    pub const DOC_QUOTE_LIST_VERBOSE: &str =
        "Example: (quote-list '(1 2 3)) => ((quote 1) (quote 2) (quote 3))";

    pub const TRANSFORM_IN_SINGLETON_LIST: &str = "transform-in-singleton-list";
    pub const DOC_TRANSFORM_IN_SINGLETON_LIST: &str =
        "Transform a list of elements into a list a singleton.";
    pub const DOC_TRANSFORM_IN_SINGLETON_LIST_VERBOSE: &str =
        "Example: (transform-in-singleton-list 1 2 3) => ((1)(2)(3))";

    //MACROS
    pub const AND: &str = "and";
    pub const DOC_AND: &str = "[Macro] Return true if all expressions are true.";
    pub const MACRO_AND: &str = "(lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (len args) 1)
                                            (car args)
                                            `(if ,(car args)
                                                 ,(cons 'and (cdr args))
                                                 nil))))";

    pub const OR: &str = "or";
    pub const DOC_OR: &str = "[Macro] Return true if at least one of the expression is true.";
    pub const MACRO_OR: &str = "(lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (len args) 1)
                                            (car args)
                                            `(if ,(car args)
                                                  true
                                                  ,(cons 'or (cdr args))))))";

    pub const CAAR: &str = "caar";
    pub const DOC_CAAR: &str = "[Macro] Combine two car.";
    pub const MACRO_CAAR: &str = "(lambda (x) `(car (car ,x)))";

    pub const CADR: &str = "cadr";
    pub const DOC_CADR: &str = "[Macro] Combine car and cdr.";
    pub const MACRO_CADR: &str = "(lambda (x) `(car (cdr ,x)))";

    pub const CDAR: &str = "cdar";
    pub const DOC_CDAR: &str = "[Macro] Combine cdr and car.";
    pub const MACRO_CDAR: &str = "(lambda (x) `(cdr (car ,x)))";

    pub const CDDR: &str = "cddr";
    pub const DOC_CDDR: &str = "[Macro] Combine cdr and cdr.";
    pub const MACRO_CDDR: &str = "(lambda (x) `(cdr (cdr ,x)))";

    pub const CAADR: &str = "caadr";
    pub const DOC_CAADR: &str = "[Macro] Combine car,car and cdr.";
    pub const MACRO_CAADR: &str = "(lambda (x) `(car (car (cdr ,x))))";

    pub const CADDR: &str = "caddr";
    pub const DOC_CADDR: &str = "[Macro] Combine car, cdr and cdr.";
    pub const MACRO_CADDR: &str = "(lambda (x) `(car (cdr (cdr ,x))))";

    pub const CADAR: &str = "cadar";
    pub const DOC_CADAR: &str = "[Macro] Combine car, cdr and car";
    pub const MACRO_CADAR: &str = "(lambda (x) `(car (cdr (car ,x))))";

    pub const CDADR: &str = "cdadr";
    pub const DOC_CDADR: &str = "[Macro] Combine cdr, car and cdr";
    pub const MACRO_CDADR: &str = "(lambda (x) `(cdr (car (cdr ,x))))";

    pub const CADADR: &str = "cadadr";
    pub const DOC_CADADR: &str = "[Macro] Combine car, cdr, car and cdr";
    pub const MACRO_CADADR: &str = "(lambda (x) `(car (cdr (car (cdr ,x)))))";

    pub const CADADDR: &str = "cdaddr";
    pub const DOC_CADADDR: &str = "[Macro] Combien car, cdr, car, cdr and cdr";
    pub const MACRO_CADADDR: &str = "(lambda (x) `(car (cdr (car (cdr (cdr ,x))))))";

    pub const AWAIT_ASYNC: &str = "await-async";
    pub const DOC_AWAIT_ASYNC: &str = "[Macro] Combine await and async";
    pub const MACRO_AWAIT_ASYNC: &str = "(lambda (x) `(await (async ,x)))";

    pub const APPLY: &str = "apply";
    pub const DOC_APPLY: &str = "[Macro] apply a function to a list of args";
    pub const DOC_APPLY_VERBOSE: &str = "Example: (apply + '(1 2 3)) => (+ 1 2 3)";
    pub const MACRO_APPLY: &str = "(lambda (f args)
                                          (cons f args))";

    pub const COND: &str = "cond";
    pub const DOC_COND: &str ="[Macro] List of tuples {boolean expression, expression}, where if the boolean expression is true, then the expression is evaluated and is result of the whole expression.";
    pub const DOC_COND_VERBOSE: &str =
        "Example of expression: (cond ((< temp 0) 'cold) (else 'good))";
    pub const MACRO_COND: &str = "(lambda exprs
    (if (null? exprs)
        nil
        (if (= (caar exprs) 'else)
            (cadar exprs)
            `(if ,(caar exprs)
                ,(cadar exprs)
                ,(cons cond (cdr exprs))))))";

    pub const FOR: &str = "for";
    pub const DOC_FOR: &str = "[Macro] Not yet implemented";
    /*pub const MACRO_FOR: &str = "(defmacro for (lambda args \
    (let ((_i_ (get-list args 0)) \
            (_list_ (get args 2)) \
            (_body_ (get args 3))) \
        `(let ((_f_loop_ (lambda args \
            (if (null? args) \
                nil \
                (let ((,_i_ (car args))) \
                    (begin \
                        ,_body_ \
                        (_f_loop_ (cdr args)))))))) \
            (_f_loop_ ,_list_)))))";*/

    pub const WHILE: &str = "while";
    pub const DOC_WHILE: &str = "[Macro] Not yet implemented.";
    /*pub const MACRO_WHILE: &str = "(defmacro while
    (lambda (c b)
        `(begin
            (define __loop__
                (lambda nil
                    (if ,c
                        (begin
                            ,b
                            (__loop__))
                    nil)))
            (__loop__))))";*/

    pub const LOOP: &str = "loop";
    pub const DOC_LOOP: &str = "[Macro] Evaluate infinitely an expression.";
    pub const MACRO_LOOP: &str = "(lambda (b)
        `(begin 
            (define __loop__
                (lambda nil 
                    (begin 
                        ,b
                        (__loop__))))
            (__loop__)))";

    pub const LET: &str = "let";
    pub const DOC_LET: &str = "[Macro] Abstract variable binding in functional programming.";
    pub const DOC_LET_VERBOSE: &str = "Example: (let ((a 1) (b 2)) (+ a b)) => 3";
    pub const MACRO_LET: &str = "(lambda (bindings body)
        (begin
            (define unzipped (unzip bindings))
            (define keys (car unzipped))
            (define values (cadr unzipped))
            (cons `(lambda ,keys
                        ,body)
                    values)))";

    pub const LET_STAR: &str = "let*";
    pub const DOC_LET_STAR: &str = "[Macro] Abstract variable binding in functional programming.\
    The difference with let is that you can bind variables in function of previously bound variables.";
    pub const DOC_LET_STAR_VERBOSE: &str = "Example: (let ((a 1) (b (+ a 1))) (+ a b)) => 3";
    pub const MACRO_LET_STAR: &str = "(lambda (bindings body)
        (if (= (len bindings) 1)
            (cons `(lambda ,(list (caar bindings))
                           ,body)
                    (cdar bindings))
            (cons `(lambda ,(list (caar bindings))
                            (let* ,(cdr bindings) ,body))
                        (cdar bindings))))";

    //lambdas
    pub const COMBINE: &str = "combine";
    pub const DOC_COMBINE: &str = "[Macro] Not yet implemented";
    /*pub const LAMBDA_COMBINE:  &str = "(define combine (lambda (f)
    (lambda (x y)
            (if (null? x) (quote ())
                (f (list (car x) (car y))
                ((combine f) (cdr x) (cdr y)))))))";*/

    pub const ZIP: &str = "zip";
    pub const DOC_ZIP: &str =
        "[Lambda] Zip two lists together by combining elements of the two lists by pair.";
    pub const DOC_ZIP_VERBOSE: &str =
        "Example: (zip '(1 2 3 4 5) '(6 7 8 9 10)) => ((1 6) (2 7) (3 8) (4 9) (5 10))";
    pub const LAMBDA_ZIP: &str = "(lambda (l1 l2)\
                                                        (if (or (null? l1)\
                                                                (null? l2))\
                                                             nil\
                                                             (cons (list (car l1)\
                                                                         (car l2))\
                                                                   (zip (cdr l1)\
                                                                        (cdr l2))))))";

    pub const UNZIP: &str = "unzip";
    pub const DOC_UNZIP: &str = "[Lambda] Unzip a zipped list.";
    pub const DOC_UNZIP_VERBOSE: &str =
        "Example: (unzip '((1 6) (2 7) (3 8) (4 9) (5 10))) => ((1 2 3 4 5) (6 7 8 9 10))";
    pub const LAMBDA_UNZIP: &str = "(lambda (lists)
                (begin
                    (define firsts 
                        (lambda (lists)
                            (if (null? lists)
                                nil
                                (cons (caar lists)
                                        (firsts (cdr lists))))))
                    (define seconds 
                        (lambda (lists)
                            (if (null? lists)
                            nil
                            (cons (cadar lists)
                                    (seconds (cdr lists))))))
                    (list (firsts lists) (seconds lists))))";

    pub const MAPF: &str = "mapf";
    pub const DOC_MAPF: &str =
        "[Lambda] Apply a function to all elements of a list, and return a list of all the results";
    pub const DOC_MAPF_VERBOSE: &str = "Example: (mapf square '(1 2 3)) => (1 4 9)";
    pub const LAMBDA_MAPF: &str = "(lambda (f seq)
         (if (null? seq)
         nil
         (cons (eval (cons f (car seq))) (mapf f (cdr seq)))))";

    /*pub const LAMBDA_ARBITRARY: &str = "(define arbitrary
    (lambda args
        (cond ((= (len args) 1) ; default case
               (car (first args)))
              ((= (len args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases";*/

    pub const PAR: &str = "par";
    pub const DOC_PAR: &str = "[Lambda] Not yet implemented!";
    /*pub const LAMBDA_PAR: &str = "(define par (lambda l
    (mapf await (mapf async l))))";*/

    pub const REPEAT: &str = "repeat";
    pub const DOC_REPEAT: &str = "[Lambda] Repeat the evaluation of an expression n times.";
    pub const LAMBDA_REPEAT: &str = "(lambda (e n)
 (if (> n 0)
     (begin
         (eval e)
         (repeat e (- n 1)))))";

    pub const RETRY_ONCE: &str = "retry-once";
    pub const DOC_RETRY_ONCE: &str =
        "[Lambda] Evaluate an expression, and if the result is an error,\
     evaluates it again and return the result of the second evaluation.";
    pub const LAMBDA_RETRY_ONCE: &str = "(lambda (e)
 (begin
     (define __r__ (eval e))
     (if (err? __r__)
         (eval e)
         __r__)))";

    pub const AWAIT_INTERRUPT: &str = "await-interrupt";
    pub const DOC_AWAIT_INTERRUPT: &str = "[Lambda] Await on a interruptible handle.";
    pub const LAMBDA_AWAIT_INTERRUPT: &str = "(lambda (__h__)
 (u! 
     (begin
         (define __r__ (i! (await __h__)))
         (if (interrupted? __r__)
             (interrupt __h__)
             __r__))))";
}

pub mod list {
    pub const MOD_LIST: &str = "list";
    pub const DOC_MOD_LIST: &str =
        "Collection of functions to manipulate LValue::List and LValue::Nil.";

    pub const FN_LIST: &str = "list";
    pub const DOC_LIST: &str = "Creat a list from the argument.";
    pub const DOC_LIST_VERBOSE: &str = "(list 1 ten 4.0) => (1 ten 4.0)";

    pub const FIRST: &str = "first";
    pub const DOC_FIRST: &str = "Return the first element of a list or nil.";

    pub const SECOND: &str = "second";
    pub const DOC_SECOND: &str = "Return the second element of a list or nil.";

    pub const THIRD: &str = "third";
    pub const DOC_THIRD: &str = "Return the third element of a list or nil.";

    pub const REST: &str = "rest";
    pub const DOC_REST: &str = "Same as cdr.";

    pub const CAR: &str = "car";
    pub const DOC_CAR: &str =
        "Takes a list and return the first element is the list is not empty, nil otherwise.";

    pub const CDR: &str = "cdr";
    pub const DOC_CDR: &str = "Takes a list and return a list without the first element.";

    pub const APPEND: &str = "append";
    pub const DOC_APPEND: &str = "Takes two list and return a list merging both.";

    pub const LAST: &str = "last";
    pub const DOC_LAST: &str =
        "Takes a list of at least one element and return the last element. Nil otherwise";

    pub const MEMBER: &str = "member";
    pub const DOC_MEMBER: &str = "Takes two arguments of which the second must be a list. \
if the first argument is a member of the second argument,\
and then it returns the remainder of the list beginning with the first argument.";

    pub const REVERSE: &str = "reverse";
    pub const DOC_REVERSE: &str =
        "Takes a list and return a list with all elements reversed in order";

    pub const GET_LIST: &str = "get-list";
    pub const DOC_GET_LIST: &str = "Return the nth element of the list.";
    pub const DOC_GET_LIST_VERBOSE: &str = "Example: (get-list '(1 2 3) 0) => 0";

    pub const SET_LIST: &str = "set-list";
    pub const DOC_SET_LIST: &str = "Set the nth element of the list and return a new list";
    pub const DOC_SET_LIST_VERBOSE: &str = "Example: (set-list '(1 2 3) 0 6) => (6 2 3)";

    pub const CONS: &str = "cons";
    pub const DOC_CONS: &str = "Create a list by adding pushing the first into the second. The behavior depends on the kind of the second element.";
    pub const DOC_CONS_VERBOSE: &str = "Examples:\n\
    \t- (cons 1 nil) => (1)\n\
    \t- (cons nil 1) => (nil 1)\n\
    \t- (cons 1 '(2 3)) => (1 2 3)\n\
    \t- (cons '(1 2) '(3 4)) ((1 2) 3 4)";

    pub const INTERSECTION: &str = "intersection";
    pub const DOC_INTERSECTION: &str = "Return the common elements of the given lists.";
    pub const DOC_INTERSECTION_VERBOSE: &str = "Example: (intersection (1 2 3) (3 4) (3 5)) => (3)";
}

pub mod set {
    pub const MOD_SET: &str = "set";
    pub const DOC_MOD_SET: &str = "Collection of functions to manipulates sets (list or map)";

    pub const SET: &str = "set";
    pub const DOC_SET: &str= "Set an element in a list or a map. See set-list and set-map for specific behavior documentation.";

    pub const GET: &str = "get";
    pub const DOC_GET: &str= "Get an element in a list or a map. See get-list and get-map for specific behavior documentation.";

    pub const EMPTY: &str = "empty";
    pub const DOC_EMPTY: &str = "Return true if the a list or map is empty.";

    pub const LEN: &str = "len";
    pub const DOC_LEN: &str = "Return the number of element in a list or a map.";
}

/// Set of keywords and documentation for basic mathematical functions.
pub mod basic_math {
    //Mathematical functions
    pub const MOD_BASIC_MATH: &str = "basic-math";
    pub const DOC_MOD_BASIC_MATH: &str = "Collection of basic mathematical and logic functions.";

    pub const NOT: &str = "not";
    pub const DOC_NOT: &str = "Return the opposite of the boolean expression.";

    pub const NOT_SHORT: &str = "!";
    pub const DOC_NOT_SHORT: &str = "Short version of not.";

    pub const NEQ: &str = "!=";
    pub const DOC_NEQ: &str = "Return true if two LValues are different.";

    pub const ADD: &str = "+";
    pub const DOC_ADD: &str = "Takes 2+ arguments. Return the addition.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";

    pub const SUB: &str = "-";
    pub const DOC_SUB: &str =
        "Takes 2 arguments. Return the substraction of the first by the second.\
return an error if inputs are not numbers or there is wrong numbers of arguments";

    pub const MUL: &str = "*";
    pub const DOC_MUL: &str = "Takes 2+ arguments. Return the multiplication.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";

    pub const DIV: &str = "/";
    pub const DOC_DIV: &str = "Takes 2 arguments. Return the division of the first by the second.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";

    //Comparison
    pub const GT: &str = ">";
    pub const DOC_GT: &str = "Takes 2 arguments. Return *true* if the first is greater than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

    pub const LT: &str = "<";
    pub const DOC_LT: &str = "Takes 2 arguments. Return *true* if the first is less than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

    pub const GEQ: &str = ">=";
    pub const DOC_GEQ: &str = "Takes 2 arguments. Return *true* if the first is greater or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

    pub const LEQ: &str = "<=";
    pub const DOC_LEQ: &str = "Takes 2 arguments. Return *true* if the first is less or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

    pub const EQ: &str = "=";
    pub const DOC_EQ: &str =
        "Takes 2 arguments. Return true if two arguments are equal. False otherwise.";
}

pub mod error {
    pub const MOD_ERROR: &str = "error";
    pub const DOC_MOD_ERROR: &str = "Collection of functions to manipulate LValue::Err.";

    pub const CHECK: &str = "check";
    pub const DOC_CHECK: &str = "Return an LValue::Err if the LValue if false";

    pub const FN_ERR: &str = "err";
    pub const DOC_FN_ERR: &str = "Return a LValue::Err";

    pub const IS_ERR: &str = "err?";
    pub const DOC_ERR: &str = "Return an LValue::Err(LValue)";
    pub const DOC_IS_ERR: &str =
        "Return true if the argument is an lvalue of the form (err <expr>), false otherwise.";

    pub const IS_INTERRUPTED: &str = "interrupted?";
    pub const DOC_IS_INTERRUPTED: &str =
        "Return true if the expression is the result of an interruption.";

    pub const INTERRUPTED: &str = "interrupted";
}

pub mod map {
    pub const MOD_MAP: &str = "map";
    pub const DOC_MOD_MAP: &str = "Collection of functions to manipulate LValue::Map.";

    pub const FN_MAP: &str = "map";
    pub const DOC_FN_MAP: &str = "Return a map from from a list of pairs.";
    pub const DOC_FN_MAP_VERBOSE: &str = "Example: (map (quote ((ten . 10) (twenty . 20))))";

    pub const GET_MAP: &str = "get-map";
    pub const DOC_GET_MAP: &str =
        "Takes a map and a key as argument, and return the value associated.";
    pub const DOC_GET_MAP_VERBOSE: &str = "Example: Here is an example in the repl\n\
                                 \t>> (define m (map (quote ((ten . 10) (twenty . 20)))))\n\
                                 \t>> (get-map m ten)\n\
                                 \tLI>> 10";

    pub const SET_MAP: &str = "set-map";
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

    pub const REMOVE_MAP: &str = "remove-map";
    pub const DOC_REMOVE_MAP: &str = "Remove a key from a map and return the new map.";

    pub const REMOVE_KEY_VALUE_MAP: &str = "remove-key-value-map";
    pub const DOC_REMOVE_KEY_VALUE_MAP: &str= "Remove a pair key-value from a map if the value corresponds to the key. Return the new map.";

    pub const UNION_MAP: &str = "union-map";
    pub const DOC_UNION_MAP: &str = "Return the union of two maps.";
}
/// Set of keywords and documentations for predicated on the kind of LValues.
pub mod predicate {
    pub const MOD_PREDICATE: &str = "predicate";
    pub const DOC_MOD_PREDICATE: &str = "Collection of functions manipulating the kind of LValue.";

    pub const KIND: &str = "kind";
    pub const DOC_KIND: &str = "Return the kind of the LValue.";

    pub const IS_NIL: &str = "null?";
    pub const DOC_IS_NIL: &str = "Return true if symbol is LValue::Nil or empty list.";

    pub const IS_NUMBER: &str = "number?";
    pub const DOC_IS_NUMBER: &str = "Return true if the LValue is a LValue::Number.";

    pub const IS_FLOAT: &str = "float?";
    pub const DOC_IS_FLOAT: &str = "Return true if the LValue is a LValue::Number(LNumber::Float).";

    pub const IS_INT: &str = "int?";
    pub const DOC_IS_INT: &str = "Return true if the LValue is a LValue::Number(LNumber::Int).";

    pub const IS_BOOL: &str = "bool?";
    pub const DOC_IS_BOOL: &str = "Return true if the LValue is a LValue::Bool.";

    pub const IS_SYMBOL: &str = "symbol?";
    pub const DOC_IS_SYMBOL: &str = "Return true if the LValue is a LValue::Symbol.";

    pub const IS_STRING: &str = "string?";
    pub const DOC_IS_STRING: &str = "Return true if the LValue is a LValue::String.";

    pub const IS_LIST: &str = "list?";
    pub const DOC_IS_LIST: &str = "Return true if the LValue is a LValue::List.";

    pub const IS_MAP: &str = "map?";
    pub const DOC_IS_MAP: &str = "Return true if the LValue is a LValue::Map.";

    pub const IS_LAMBDA: &str = "lambda?";
    pub const DOC_IS_LAMBDA: &str = "Return true if the LValue is a LValue::Lambda.";

    pub const IS_PRIMITIVE: &str = "primitive?";
    pub const DOC_IS_PRIMITIVE: &str = "Return true if the LValue is a primitive";

    pub const IS_HANDLE: &str = "handle?";
    pub const DOC_IS_HANDLE: &str = "Return true if the LValue is a handle";

    pub const IS_FN: &str = "fn?";
    pub const DOC_IS_FN: &str = "Return true if the LValue is a LValue::Fn.";

    pub const IS_MUT_FN: &str = "mut-fn?";
    pub const DOC_IS_MUT_FN: &str = "Return true if the LValue is a LValue::MutFn.";

    pub const IS_ASYNC_FN: &str = "async-fn?";
    pub const DOC_IS_ASYNC_FN: &str = "Return true if the LValue is a LValue::AsyncFn.";

    pub const IS_ASYNC_MUT_FN: &str = "async-mut-fn?";
    pub const DOC_IS_ASYNC_MUT_FN: &str = "Return true if the LValue is a LValue::AsyncMutFn.";

    pub const IS_PAIR: &str = "pair?";
    pub const DOC_IS_PAIR: &str = "Return true if the list is not empty, false otherwise.";

    pub const IS_EQUAL: &str = "equal?";
    pub const DOC_IS_EQUAL: &str = "Return true if two LValues are equal, false otherwise.";
}

/// Collection of functions using time: sleep, time, my-time etc.
pub mod time {
    /// Label of the context used the env to manage time
    pub const MOD_TIME: &str = "time";
    pub const DOC_MOD_TIME: &str =
        "Context used to reason on time. Contains functions as sleep, time, my-time, etc.";

    pub const __SLEEP__: &str = "__sleep__";
    pub const DOC___SLEEP__: &str =
        "Sleep for the given time in seconds. Returns a handle that should be awaited on.";

    /// Wrapper around the function sleep
    pub const SLEEP: &str = "sleep";
    pub const DOC_SLEEP: &str =
        "Sleep for the given time in seconds. The function can be interrupted.";
    pub const LAMBDA_SLEEP: &str = "(lambda (n) (u! (await-interrupt (__sleep__ n))))";

    pub const TIME: &str = "time";
    pub const DOC_TIME: &str =
        "Return the absolute time of the system in function of predefined shift. The result is a map containing the fields second, minute, hour, day, month, year.";

    pub const MY_TIME: &str = "my-time";
    pub const DOC_MY_TIME: &str =
        "Return the relative time of the system in function of the last reset. The time is given in seconds.";
}

pub mod sort {
    pub const MOD_SORT: &str = "sort";
    pub const DOC_MOD_SORT: &str = "Collection of functions to sort list and map";
}

pub mod eval_static {
    pub const MOD_EVAL_STATIC: &str = "eval_static";
    pub const DOC_MOD_EVAL_STATIC: &str =
        "Collection of functions evaluating statically expressions";

    pub const EVAL_STATIC: &str = "eval_static";
    pub const DOC_EVAL_STATIC: &str = "Evaluate static expression";
}

pub mod string {
    pub const MOD_STRING: &str = "string";
    pub const DOC_MOD_STRING: &str = "Collection of functions to manipulate string.";

    pub const CONCATENATE: &str = "concatenate";
    pub const DOC_CONCATENATE: &str =
        "Creates a string from a list of LValue by concatening their equivalent in string.";
}

pub mod advanced_math {
    pub const MOD_ADVANCED_MATH: &str = "math";
    pub const DOC_MOD_ADVANCED_MATH: &str =
        "Module handling mathematical functions for basic arithmetic operations and comparisons.";

    //Trigonometry
    pub const SIN: &str = "sin";
    pub const DOC_SIN: &str = "Takes 1 argument. Return the sinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";

    pub const COS: &str = "cos";
    pub const DOC_COS: &str = "Takes 1 argument. Return the cosinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";

    pub const SQRT: &str = "sqrt";
    pub const DOC_SQRT: &str = "Takes 1 argument. Return the square root of the number.";

    pub const POW: &str = "pow";
    pub const DOC_POW: &str =
        "Takes 2 arguments. Return the first argument to the power of the second argument.";

    pub const SQUARE: &str = "square";
    pub const DOC_SQUARE: &str = "Takes 1 argument. Return the first argument to the power of 2.";

    pub const ABS: &str = "abs";
    pub const DOC_ABS: &str = "Compute the absolute value of a number.";

    pub const RAND_INT_IN_RANGE: &str = "rand-int-in-range";
    pub const DOC_RAND_INT_IN_RANGE: &str = "Returns a random int between two numbers";
    pub const DOC_RAND_INT_IN_RANGE_VERBOSE: &str = "Example:\n(rand-int-in-range 1 10)\n=> 2";

    pub const RAND_FLOAT_IN_RANGE: &str = "rand-float-in-range";
    pub const DOC_RAND_FLOAT_IN_RANGE: &str = "Returns a random float between two numbers";
    pub const DOC_RAND_FLOAT_IN_RANGE_VERBOSE: &str =
        "Example:\n(rand-float-in-range 1 10)\n=> 2.32511455...";

    //Constants
    pub const PI: &str = "pi";
    pub const DOC_PI: &str = "Pi constant";
}

pub mod io {
    pub const MOD_IO: &str = "io";
    pub const DOC_MOD_IO: &str = "Module than handles input/output functions.";

    pub const PRINT: &str = "print";
    pub const DOC_PRINT: &str = "Print in stdout a LValue.";
    pub const DOC_PRINT_VERBOSE: &str = "Takes a list of arguments and print them in stdout.";

    pub const __READ__: &str = "__read__";
    pub const DOC___READ__: &str = "Read a file an evaluate it";
    pub const DOC___READ___VERBOSE: &str = "Takes the name of the file as argument.\n\
                                Note: The file path is relative to the path of the executable.\n\
                                Return an error if the file is not found or there is a problem while parsing and evaluation.";
    pub const WRITE: &str = "write";
    pub const DOC_WRITE: &str = "Write a LValue to a file";
    pub const DOC_WRITE_VERBOSE: &str = "Takes two arguments: the name of the file and the LValue\n\
                                 Note: The path of the file is relative to the path of the executable";

    pub const READ: &str = "read";
    pub const DOC_READ: &str = "[Macro] Wrapper around __read__";
    pub const MACRO_READ: &str = "(lambda (x)\
        `(eval (parse (__read__ ,x))))";
}
