
;par
    
(define par (lambda l
    (mapf await (mapf async l))))



;wait-for
;monitor
(define monitor 
    (lambda (e)
        (wait-for `(= ,e nil))))
;repeat
(define repeat (lambda (e n)
    (if (= n 1)
        (begin
            (eval e)
            (repeat e (- n 1))))))
;retry-once
(define retry-once (lambda (e)
    (begin
        (define __r__ (eval e))
        (if (err? __r__)
            (eval e)
            __r__))))

;run-monitoring
(define run-monitoring
    (lambda (b m)
        (race b `(monitor ,m))
))

;sleep : __sleep__ return a LFuture
(define sleep 
    (lambda (n)
        (u! 
            (await-interrupt (__sleep__ n)))))

(define await-interrupt
    (lambda (__h__)
    (u! 
        (begin
            (define __r__ (i! (await __h__)))
            (if (interrupted? __r__)
                (interrupt __h__)
                __r__)))))

(define exec-command (lambda l)
    (u! 
        (await-interrupt (eval (cons `(__exec-command__ ,l))))))

(define wait-for (lambda (e)
    (u! 
        (await-interrupt (__wait-for__ e)))))


(define acquire (lambda args
    (u!
        (await-interrupt (enr (cons '__acquire__ args))))))

(await
    (race
        (begin
            (await (sleep 1))
            (await (sleep 2))
        )
        (await (sleep 4))
    )
)

(do
    (define h (sleep 1))
    (print 1)
    (await (sleep 2))
    (print 2)
    (await h)
    (print 3)
)