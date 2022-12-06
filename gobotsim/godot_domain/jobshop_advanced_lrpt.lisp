(begin
    (def-lambda 
        remaining-time (lambda ?p
            (eval (cons '+ (cadr (unzip (package.processes_list ?p)))))))

    (def-task t_process_package (:params (?p package)))

    (def-method m_process_to_do_r
        (:task t_process_package)
        (:params (?p package))
        (:pre-conditions (! (null? (package.processes_list ?p))))
        (:score 0)
        (:body
            (do
                (define ?m
                    (arbitrary (find_machines_for_process
                        (caar (unzip (package.processes_list ?p))))))
                (t_process_on_machine ?p ?m)
                (t_process_package ?p))))                       

    (def-method m_no_more_process
        (:task t_process_package)
        (:params (?p package))
        (:pre-conditions (null? (package.processes_list ?p)))
        (:score 0)
        (:body
            (do
                (print "package process of " ?p " is done")
                (define h_r (acquire-in-list (instances robot)))
                (define ?r (first h_r))
                (t_carry_to_machine ?r ?p (find_output_machine)))))
    
    (def-task t_process_on_machine (:params (?p package) (?m machine)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define r-time (remaining-time ?p))
                (define h_m (acquire ?m `(:priority ,r-time)))
                (define h_r (acquire-in-list (instances robot) `(:priority ,r-time)))
                (define ?r (first h_r))
                (t_carry_to_machine ?r ?p ?m)
                (release (second h_r))
                (process ?m ?p))))
)
