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

(defmacro debug_ompas (lambda (arg)
    `(__debug_ompas__ ',arg)))

(lambda args
    (begin
        (define __l__ (lambda (l)
        (if (null? l)
        nil
            (cons
                (cons (caar l) (list (cdar l)))
                (__l__ (cdr l))))))
        `(map
            (quote ,(append (list '(:model-type pddl)) (__l__ args))))))


(lambda args
    (if (= (len args) 1)
        (car args)
        `(begin
            (define __r__ ,(car args))
            (if (err? __r__)
                __r__
                (do_t (cdr args))
            ))))

(defmacro do_t (lambda args
    (if (= (len args) 1)
        (car args)
        `(begin
            (define __r__ ,(car args))
            (if (err? __r__)
                __r__
                ,(cons do_t (cdr args))
            )))))
(begin 
    (define __r__ a)
    (if (err? __r__)
        __r__ 
        (begin 
            (define __r__ b) 
            (if (err? __r__) 
                __r__ 
                c))))
