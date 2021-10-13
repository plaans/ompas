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


`List([Symbol("list"), Symbol("sf"), List([CoreOperator(DefLambda), List([Symbol("?a"), Symbol("?b"), Symbol("?c")]), List([Symbol("rae-get-state-variable"), List([CoreOperator(Quote), Symbol("sf")]), Symbol("?a"), Symbol("?b"), Symbol("?c")])]), List([CoreOperator(DefLambda), List([Symbol("?a"), Symbol("?b"), Symbol("?c")]), List([Symbol("get-map"), Symbol("state"), List([Fn(label: "list"mod : 0), List([CoreOperator(Quote), Symbol("sf")]), Symbol("?a"), Symbol("?b"), Symbol("?c")])])])])


`List([Symbol("list"), Symbol("sf"), List([CoreOperator(DefLambda), List([Symbol("?a"), Symbol("?b"), Symbol("?c")]), List([Symbol("rae-get-state-variable"), List([CoreOperator(Quote), Symbol("sf")]), Symbol("?a"), Symbol("?b"), Symbol("?c")])]), List([CoreOperator(DefLambda), List([Symbol("?a"), Symbol("?b"), Symbol("?c")]), List([Symbol("get-map"), Symbol("state"), List([Symbol("list"), List([CoreOperator(Quote), Symbol("sf")]), Symbol("?a"), Symbol("?b"), Symbol("?c")])])])])`