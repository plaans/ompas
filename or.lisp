(defmacro or  (lambda (a b) (quasiquote (if (unquote a) true (unquote b)))))

(define and (lambda args (if (none? args) true (if (car args) (and (cdr args)) false))))
