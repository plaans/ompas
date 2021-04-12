(defmacro or  (lambda (a b) (quasiquote (if (unquote a) true (unquote b)))))
