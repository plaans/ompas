(begin
    (def-task t_process_package (:params (?p package)))

    (def-method m_process_to_do_r
        (:task t_process_package)
        (:params (?p package))
        (:pre-conditions (!(null? (package.processes_list ?p))))
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
                (define h_r (await (acquire-in-list (instances robot))))
                (define ?r (first h_r))
                (t_carry_to_machine ?r ?p (find_output_machine)))))
    
    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define h_m (await (acquire ?m)))
                (define h_r (await (acquire-in-list (instances robot))))
                (define ?r (first h_r))
                (t_carry_to_machine ?r ?p ?m)
                (release (second h_r))
                (t_process ?m ?p ?d))))

)
