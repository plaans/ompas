(lambda (id arbitrary)
    (begin
        (def-label 'arbitrary ,id)
        ,arbitrary
    )
)

(lambda __task__
        (begin
            (define _m_ (enr (cons 'refine __task__)))
            (if (err? _m_)
                _m_
                    (begin
                        (if (err? (eval _m_))
                            (retry)
                            (set-success-for-task))))))


(begin
    (activate_log log-ompas)
    (start)
    (trigger-task t_jobshop)
)


(lambda __task__
        (begin
            (define _r_ (enr (cons 'refine __task__)))
            (if (err? _r_)
                _r_
                (let ((_m_ (first _r_))
                      (_id_ (second _r_)))
                    (begin
                        (def_process_id _id_)
                        (if (err? (eval _m_))
                            (retry)
                            (set-success-for-task)))))))

(define _loop_
    (lambda _body_
        (begin
            (define _r_ (eval _body_))
            (_loop_ _body_))))