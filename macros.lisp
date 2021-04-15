(begin
    (defmacro and2 (lambda (a b)
        (quasiquote
            (if (unquote a)
                (unquote b)
                false)
            )))
    (defmacro or2  (lambda (a b)
        (quasiquote
            (if (unquote a)
                true
                (unquote b)
            ))))
    (defmacro not (lambda a
        (quasiquote
            (if (unquote a)
                false
                true
                ))))
)