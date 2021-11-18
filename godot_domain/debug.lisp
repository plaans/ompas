(begin
    (def-lambda '(available_robots
            (lambda nil
                (begin
                    (define __l_available_robots__
                        (lambda (l)
                            (if (null? l)
                                nil
                                (if (not (locked? (car l)))
                                    (cons (car l) (__l_available_robots__ (cdr l)))
                                    (__l_available_robots__ (cdr l))))))
                    (__l_available_robots__ (robots))))))
    (def-task t)
    (def-method m
    '((:task t)
        (:params)
        (:pre-conditions (!= (available_robots) nil))
        (:effects nil)
        (:score 0)
        (:body true)))
)