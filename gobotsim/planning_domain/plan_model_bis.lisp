(begin
    (def-task t_jobshop)

    (def-method m1
       (:task t_jobshop)
       (:score 0)
       (:body
           (do
               ;(mapf new-resource (instances robot))
               ;(mapf new-resource (instances machine))
               ;(define f2 (async (t_check_rob_bat)))
               (define tasks 
                   (mapf (lambda (?p) 
                       (do
                           (define tasks (mapf (lambda (process)
                               `(t_process_on_machine ,?p 
                                   (arbitrary ',(find_machines_for_process (car process)))                                   ))
                               (package.all_processes ?p)))
                            (define last_task
                                `(begin
                                    (define ?r (arbitrary (instances robot)))
                                    (define h_r (acquire ?r))
                                    ,(define ?m (find_output_machine))
                                    ,?m
                                    (t_carry_to_machine ?r ,?p ,?m)))
                            (define tasks (append tasks (list last_task)))
                            ;(print tasks)
                            `(apply seq ',tasks)))
                        (instances package)))
               (define h (apply par tasks)))))

    (def-task t_carry_to_machine (:params (?r robot) (?p package) (?m machine)))

    (def-task t_process_on_machine (:params (?p package) (?m machine)))
    (def-task-om-model t_process_on_machine
            (:params (?p package) (?m machine))
            (:body
                (do
                    (define rh (acquire ?m))
                    ;(sleep 1)
                    (release rh)
                    )))

    (def-task test)
        (def-method m_test
            (:task test)
            (:body
                (begin
                    (define a 'test)
                    (assert a 2)
                    (define b 'test2)
                    a)))
)