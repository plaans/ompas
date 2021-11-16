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