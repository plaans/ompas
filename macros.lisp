(defmacro and (lambda (a b) (quote (if a b false))))
(defmacro or  (lambda (a b) (quote (if a true b))))
(defmacro not (lambda a (quote (if a false true))))