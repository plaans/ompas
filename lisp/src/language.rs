pub const MOD_ROOT: &str = "mod-root";

pub mod scheme_primitives {
    /*
    LIST FUNCTIONS
     */
    pub const CAR: &str = "car";
    pub const CDR: &str = "cdr";
    pub const APPEND: &str = "append";
    pub const LAST: &str = "last";
    pub const EMPTY: &str = "empty";
    pub const LEN: &str = "length";
    pub const MEMBER: &str = "member";
    pub const REVERSE: &str = "reverse";
    pub const LIST: &str = "list";
    pub const CONS: &str = "cons";

    //Other
    pub const GET: &str = "get";
    pub const GET_MAP: &str = "get-map";
    pub const SET_MAP: &str = "set-map";
    pub const UNION_MAP: &str = "union-map";
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
    pub const ASYNC: &str = "async";
    pub const AWAIT: &str = "await";
    pub const EVAL: &str = "eval";
    pub const LET: &str = "let";
    pub const LET_STAR: &str = "let*";
    pub const MACRO_EXPAND: &str = "macro-expand";

    //Boolean
    pub const TRUE: &str = "true";
    pub const NIL: &str = "nil";
    pub const FALSE: &str = "false";
    pub const NOT: &str = "not";
    pub const NOT_SHORT: &str = "!";

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

    pub const ENV: &str = "env"; //return a list of keys of the environment

    //predicates
    pub const IS_NUMBER: &str = "number?";
    pub const IS_FLOAT: &str = "float?";
    pub const IS_INTEGER: &str = "integer";
    pub const IS_BOOL: &str = "boolean?";
    pub const IS_SYMBOL: &str = "symbol?";
    pub const IS_STRING: &str = "string?";
    pub const IS_FN: &str = "fn?";
    pub const IS_MUT_FN: &str = "mut-fn?";
    pub const IS_LIST: &str = "list?";
    pub const IS_MAP: &str = "map?";
    pub const IS_LAMBDA: &str = "lambda?";
    pub const IS_QUOTE: &str = "quote?";

    //TODO: implement following functions
    //Not yet implemented
    pub const IS_EQUAL: &str = "equal?";
    pub const IS_PAIR: &str = "pair?";
    pub const IS_NIL: &str = "null?";

    pub const FN_MAP: &str = "map";
    pub const APPLY: &str = "APPLY";
    pub const ZIP: &str = "zip";
    pub const UNZIP: &str = "unzip";
    pub const COMBINE: &str = "combine";
}

//MACRO
///Problem during expansion
pub mod scheme_macro {
    pub const MACRO_LET: &str = "(defmacro let \
                                           (lambda (bindings body) \
                                                   (begin (define unzipped (unzip bindings)) \
                                                          (define keys (car unzipped)) \
                                                          (define values (cadr unzipped)) \
                                                          (cons (quasiquote (lambda (unquote keys) \
                                                                                    (unquote body))) values))))";

    pub const MACRO_LET_STAR: &str = "(defmacro let* \
                                                (lambda (bindings body) \
                                                        (if (= (length bindings) 1) \
                                                            (cons (quasiquote (lambda (unquote (list (caar bindings))) \
                                                                                      (unquote body))) \
                                                                  (cdar bindings)) \
                                                            (cons (quasiquote (lambda (unquote (list (caar bindings))) \
                                                                                      (let* (unquote (cdr bindings)) \
                                                                                            (unquote body)))) \
                                                                  (cdar bindings)))))";

    //TODO: find a way to make it a lambda
    pub const MACRO_APPLY: &str = "(defmacro apply\
                                              (lambda (f args)\
                                                      (cons f args)))";

    pub const MACRO_AND: &str = "(defmacro and (lambda args\
                                                (if (null? args)\
                                                    nil\
                                                    (if (= (length args) 1)\
                                                        (car args)\
                                                        (quasiquote (if (unquote (car args))\
                                                                        (and (unquote (cdr args)))\
                                                                        nil))))))";

    pub const MACRO_OR: &str= "(defmacro or (lambda args\
                                                (if (null? args)\
                                                    nil\
                                                    (if (= (length args) 1)\
                                                        (car args)\
                                                        (quasiquote (if (unquote (car args))\
                                                                        true \
                                                                        (or (unquote (cdr args)))))))))";

    pub const MACRO_NEQ: &str = "(defmacro neq (lambda (a b)\
                                            (! (= a b))))";

    pub const MACRO_NEQ_SHORT: &str = "(defmacro != (lambda (a b) \
                                                (neq a b )))";

    pub const MACRO_COND: &str = "(defmacro cond (lambda exprs \
                                                (if (null? exprs) \
                                                    nil \
                                                    (if (= (caar exprs) (quote else)) \
                                                        (cadar exprs) \
                                                        (quasiquote \
                                                            (if (unquote (caar exprs)) \
                                                                (unquote (cadar exprs)) \
                                                                (cond (unquote (cdr exprs)))))))))";
}

pub mod scheme_lambda {
    pub const LAMBDA_AND: &str = "(define and (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) (and (cdr args)) nil)))))";
    pub const LAMBDA_OR: &str = "(define or (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) true (or (cdr args)))))))";

    pub const LAMBDA_COND: &str = "(define cond (lambda x \
                                            (if (null? x)\
                                                nil\
                                                (let ((a (car x))\
                                                       (tests (car a))\
                                                       (expr (cdr a)))\
                                                      (if tests expr (cond (cdr x))))))";

    pub const LAMBDA_COMBINE:  &str = "(define combine (lambda (f)
                                                                (lambda (x y)
                                                                        (if (null? x) (quote ())
                                                                            (f (list (car x) (car y))
                                                                            ((combine f) (cdr x) (cdr y)))))))";
    pub const LAMBDA_ZIP: &str = " (define zip (lambda (l1 l2)\
                                                        (if (or (null? l1)\
                                                                (null? l2))\
                                                             nil\
                                                             (cons (list (car l1)\
                                                                         (car l2))\
                                                                   (zip (cdr l1)\
                                                                        (cdr l2)))))))";

    pub const LAMBDA_CAAR: &str = "(define caar (lambda (l1) (car (car l1))))";

    pub const LAMBDA_CADR: &str = "(define cadr (lambda (l1) (car (cdr l1))))";

    pub const LAMBDA_CDAR: &str = "(define cdar (lambda (l1) (cdr (car l1))))";

    pub const LAMBDA_CDDR: &str = "(define cddr (lambda (l1) (cdr (cdr l1))))";

    pub const LAMBDA_CADAR: &str = "(define cadar (lambda (l1) (car (cdr (car l1)))))";

    pub const LAMBDA_CADADR: &str = "(define cadadr (lambda (l1) (car (cdr (car (cdr l1))))))";

    pub const LAMBDA_CDADR: &str = "(define cdadr (lambda (l1) (cdr (car (cdr l1)))))";

    pub const LAMBDA_CADADDR: &str =
        "(define cadaddr (lambda (l1) (car (cdr (car (cdr (cdr l1)))))))";

    pub const LAMBDA_UNZIP: &str = "(define unzip
        (lambda lists
                (begin
                    (define firsts (lambda lists
                                            (if (null? lists)
                                                nil
                                                (cons (caar lists)
                                                      (firsts (cdr lists))))))
                    (define seconds (lambda lists
                                            (if (null? lists)
                                            nil
                                            (cons (cadar lists)
                                                  (seconds (cdr lists))))))
                    (list (firsts lists) (seconds lists)))))";

    pub const LAMBDA_MAPF : &str = "(define mapf (lambda (f seq)\
                                                         (if (null? seq)\
                                                         nil\
                                                         (cons (f (car seq)) (mapf f (cdr seq))))))";
}
//Documentation
pub mod doc {
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
    pub const DOC_REVERSE: &str =
        "Takes a list and return a list with all elements reversed in order";
    pub const DOC_LIST: &str = "Return a list of the LValues given is argument";
    pub const DOC_CONS: &str = "Takes two objects and merges into a list.";
    pub const DOC_MAP: &str = "Return a map from from a list of pairs.";
    pub const DOC_MAP_VERBOSE: &str = "Example: (map (quote ((ten . 10) (twenty . 20))))";
    pub const DOC_GET: &str = "Takes a key as argument and return the binding if defined in the environment. Return the key otherwise.";
    pub const DOC_GET_MAP: &str =
        "Takes a map and a key as argument, and return the value associated.";
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

    pub const DOC_EQ: &str =
        "Takes 2 arguments. Return true if two arguments are equal. False otherwise.";
    pub const DOC_ADD: &str = "Takes 2+ arguments. Return the addition.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_SUB: &str =
        "Takes 2 arguments. Return the substraction of the first by the second.\
return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_MUL: &str = "Takes 2+ arguments. Return the multiplication.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_DIV: &str = "Takes 2 arguments. Return the division of the first by the second.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_GT: &str = "Takes 2 arguments. Return *true* if the first is greater than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_GE: &str = "Takes 2 arguments. Return *true* if the first is greater or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_LT: &str = "Takes 2 arguments. Return *true* if the first is less than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_LE: &str = "Takes 2 arguments. Return *true* if the first is less or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

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

    //TODO: complete documentation
    pub const DOC_IS_PAIR: &str = "todo!";
    pub const DOC_IS_EQUAL: &str = "todo!";
    pub const DOC_MACRO_EXPAND: &str = "todo!";
}
