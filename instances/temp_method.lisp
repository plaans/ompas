(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            (lambda ,params
                (get-map state (list`(quote ,label) params)))))))

(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            (lambda ,params
                (get-map state ,(cons 'list (cons `(quote ,label) params))))))))


(test-macro "(generate-state-function robot.coordinates ?r ?i)")


(define arbitrary
    (lambda args
        (cond ((= (length args) 1) ; default case
               (car (first args)))
              ((= (length args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases


(define (available_robots
        (lambda nil
            (begin
                (define __l_available_robots__
                    (lambda args
                        (if (null? args)
                            nil
                            (cons (car args) (__l_available_robots__ (cdr args))))))
                (__l_available_robots__ '(r1 r2 r3))))))


(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            ,(if (= params nil)
                `(lambda nil
                        (get-map state ',label))
                `(lambda ,params
                            (get-map state ,(cons 'list (cons `(quote ,label) params)))))))))


(define locked?
    (lambda (r)
        (get-map state (list 'locked r))))
