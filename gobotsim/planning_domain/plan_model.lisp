(begin

    (def-task t_check_rob_bat)
    (def-task-om-model t_check_rob_bat
            (:params )
            (:body nil))

    

    (def-task t_jobshop)
    ; (def-task-om-model t_jobshop
    ;     (:params )
    ;     (:body
    ;         (do
    ;             (sleep 1)))
    ; )
    (def-method m1
       (:task t_jobshop)
       (:score 0)
       (:body
           (do
               (define f2 (async (t_check_rob_bat)))
               (define tasks 
                   (mapf (lambda (?p) 
                       (do
                           (define tasks (mapf (lambda (process)
                               `(t_process_on_machine ,?p 
                                   (arbitrary ',(find_machines_for_process (car process)))
                                   ,(cadr process)
                                   ))
                               (package.all_processes ?p)))
                            (define last_task
                                 `(begin
                                     (define ?r (arbitrary (instances robot)))
                                     (define h_r (acquire ?r))
                                     (t_carry_to_machine ?r ,?p ,(find_output_machine))))
                            (define tasks (append tasks (list last_task)))
                            `(apply seq ',tasks)))
                        (instances package)))
               (define h (apply par tasks)))))

    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    ; (def-task-om-model t_process_on_machine
    ;         (:params (?p package) (?m machine) (?d int))
    ;         (:body
    ;             (do
    ;                 ;(define rh (acquire ?m))
    ;                 (sleep ?d)
    ;                 ;(release rh)
    ;                 )))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (begin
                (define ?r (arbitrary (instances robot) rand-element))
                (define h1 (acquire ?m))
                (define h2 (acquire ?r))
                (t_carry_to_machine ?r ?p ?m)
                (release h2)
                (t_process ?m ?p ?d)
                )))

    (def-task t_carry_to_machine (:params (?r robot) (?p package) (?m machine)))
    (def-task-om-model t_carry_to_machine
        (:params (?r robot) (?p package) (?m machine))
        (:body (begin (sleep 1))))

    (def-task t_process (:params (?m machine) (?p package) (?d int)))
    (def-task-om-model t_process
        (:params (?m machine) (?p package) (?d int))
        (:body (begin (sleep ?d))))
)