(begin
    (def-task t_process_package (:params (?p package)))

    (def-method m_process_to_do_r
        (:task t_process_package)
        (:params (?p package))
        (:pre-conditions (!= (package.processes_list ?p) nil))
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
        (:pre-conditions (= (package.processes_list ?p) nil))
        (:score 0)
        (:body
            (do
                (print "package process of " ?p " is done")
                (define ?r (arbitrary (instances robot) rand-element))
                (define h (await (acquire ?r)))
                (t_carry_to_machine ?r ?p (find_output_machine))
                )))
    
    (def-task t_process_on_machine (:params (?p package) (?m machine)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define ?r (arbitrary (instances robot) rand-element))
                (define h1 (await (acquire ?m)))
                (define h2 (await (acquire ?r)))
                (t_carry_to_machine ?r ?p ?m)
                (release h2)
                (process ?m ?p))))
)