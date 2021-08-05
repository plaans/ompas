(for _i_ in _list_ _body_)
=> (let ((_f_loop_ (lambda args 
            (let ((<i> (car args)))
                (begin 
                    <body>
                    (_f_loop_ (cdr args)))))))
        (_f_loop_ <list>))
;
;
;
;
;


(defmacro for (lambda args
    (let ((_i_ (get-list args 0))
            (_list_ (get-list args 2))
            (_body_ (get-list args 3)))
        `(let ((_f_loop_ (lambda args
            (if (null? args)
                nil
                (let ((,_i_ (car args)))
                    (begin
                        ,_body_
                        (_f_loop_ (cdr args))))))))
            (_f_loop_ ,_list_)))))

(for i in '(1 2 3) (print i))


(begin
    (define v nil)
    (for i in '(1 2 3) 
        (begin
            (print i)
            (print v)
            (set! v (cons i v))
            (print v)))
    v)