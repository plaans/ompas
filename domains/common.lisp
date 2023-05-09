(begin

    (def-env exec_sim (lambda (label) 
        (sleep (get __duration_table__ label))))
        
    (def-objects (unk empty object))

    (define def-prob (lambda args
        (def-env __prob_table__ (map ',args))))

    (define def-duration (lambda args)
        (def-env __duration_table__ (map ',args)))
        

    (defmacro def-prob-exec 
        (lambda __args__
            (begin
                (define format_cond 
                    (lambda __args__
                        (if (! __args__)
                            nil
                            (let ((e (car __args__))
                                  (rest (cdr __args__)))
                                (cons (let ((_l_ (car e))
                                            (_expr_ (cadr e)))
                                          `((= '_l_ _l_) _expr_))
                                    (format_cond rest))))))
                (def-env prob-exec 
                    `(lambda args
                        (let ((_l_ (car args))
                            (args (cdr args)))
                            ,(cons 'cond (cons (format_cond __args__)
                                '(else (<= (rand-float-in-range 0 1) (car (get __prob_table__ _l_)))
                                    )))))))))
    (def-prob-exec)
)