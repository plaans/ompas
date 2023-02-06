(pre-eval-expr '(begin
    (define a (quote test))
    (assert a 2)
    (define b (quote test2))
    a))

(pre-eval-expr '(begin
        (define list_packages (instances package))
        (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))))
    
(pre-eval-expr '(begin
        (define list_packages (instances package))
         list_packages))

(pre-eval-expr 
'(do
    (define list_packages (instances package))
    (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))    
    ))

(pre-eval-expr
    '(begin 
        (define seq (list (async a)))
        (define f await)
        (cons (eval (cons f (list (car seq)))) nil)
    )
)

(begin
(define par 
    (lambda _list_
        (begin
            (define _n_ (len _list_))
            (define _handle_symbols_ (mapf (lambda (n) (string::concatenate _h n _)) (range 0 (- _n_ 1))))
            (print _handle_symbols_)
            (define _tasks_ (mapf (lambda (_t_ _h_) `(define ,_h_ (async ,_t_))) _list_ _handle_symbols_))
            (print _tasks_)
            (define _awaits_ (mapf (lambda (_h_) `(await ,_h_)) _handle_symbols_))
            (cons begin (append _tasks_ _awaits_))
        
        )))

(par t1 t2))

(begin
    (define _list_ '(t0 t1))
    (define _handle_symbols_ '(h0 h1))
    (define _list_ (zip _list_ _handle_symbols_))
    ((lambda (_t_ _h_) `(define ,_h_ (async ,_t_))) '(t0 h0))
)


(define handle_symbols)

(lambda (f seq)
         (if (null? seq)
         nil
         (cons (eval (cons f (list (car seq)))) (mapf f (cdr seq)))))


(begin 
(define mapf (lambda args
    (let ((f (car args))
          (seq (cdr args)))
        (begin
            (define firsts 
                (lambda (lists)
                    (if (null? lists)
                        nil
                        (cons (caar lists)
                                (firsts (cdr lists))))))
                (define rests 
                    (lambda (lists)
                        (if (null? lists)
                            nil
                            (begin
                                (define _r_ (cdar lists))
                                (if (null? _r_)
                                    nil
                                    (cons _r_ (rests (cdr lists))))))))
            (define g_seq
                (lambda (seq)
                    (if (null? seq)
                        nil
                        (begin
                            (cons (firsts seq) (g_seq (rests seq)))))))
            (define args (g_seq seq))
            ;args
            (define _proc_ (lambda (f seq)
                (if (null? seq)
                    nil
                    (cons (eval (cons f (car seq))) (_proc_ f (cdr seq)))))
            )
            (_proc_ f args)
    ))))
        

(mapf + '(1 2) '(3 4)))


(define apply 
    (lambda args
        (enr (append (butlast args) (car (last args))))
))

(apply + '(1 2))

(defmacro apply 
    (lambda args
        (append (butlast args) (car (last args)))
))

(define seq (lambda args)
    (eval (cons begin args))
)