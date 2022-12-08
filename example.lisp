(defmacro pddl-model
    (lambda args
        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                    nil
                    (cons
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(map
                (quote ,(append (cons '(:model-type pddl) nil ) (__l__ args)))))))

(defmacro def-command-om-model
        (lambda args
            (let ((label (car args))
                  (args (cdr args)))
            `(add-command-model (union-map (map '((:name ,label))) ,(cons om-model args))))))