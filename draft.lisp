(lambda _args_
    (let ((f (car _args_))
          (_seq_ (cdr _args_)))
        (begin
            (define _firsts_ 
                (lambda (_lists_)
                    (if (null? _lists_)
                        nil
                        (cons (caar _lists_)
                                (_firsts_ (cdr _lists_))))))
                (define _rests_ 
                    (lambda (_lists_)
                        (if (null? _lists_)
                            nil
                            (begin
                                (define _r_ (cdar _lists_))
                                (if (null? _r_)
                                    nil
                                    (cons _r_ (_rests_ (cdr _lists_))))))))
            (define _g_seq__
                (lambda (_seq_)
                    (if (null? _seq_)
                        nil
                        (begin
                            (cons (_firsts_ _seq_) (_g_seq__ (_rests_ _seq_)))))))
            (define _args_ (_g_seq__ _seq_))
            ;_args_
            (define _proc_ (lambda (f _seq_)
                (if (null? _seq_)
                    nil
                    (cons (enr (cons f (car _seq_))) (_proc_ f (cdr _seq_)))))
            )
            (_proc_ f _args_)
    )))