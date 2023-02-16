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

((t_process_on_machine package0 (arbitrary (quote (machine2))))
 (t_process_on_machine package0 (arbitrary (quote (machine0))))
 (t_process_on_machine package0 (arbitrary (quote (machine1))))
 (t_process_on_machine package0 (arbitrary (quote (machine3))))
 (t_process_on_machine package0 (arbitrary (quote (machine5))))
 (t_process_on_machine package0 (arbitrary (quote (machine4))))
 (begin 
    (define h_r (acquire-in-list (instances robot)))
    (define ?r (first h_r))
    (t_carry_to_machine ?r package0 output_machine0)))