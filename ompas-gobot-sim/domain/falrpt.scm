(begin

    (load (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain/om.scm"))


    (def-lambda 
        remaining-time (lambda (?p)
            (eval (cons '+ (cadr (unzip (package.processes_list ?p)))))))

    
    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
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
                (t_process ?m ?p ?d))))

        (def-task t_output_package (:params (?p package)))
        (def-method m_output_package
            (:task t_output_package)
            (:params (?p package))
            (:body
                (do
                    (define h_r (acquire-in-list (instances robot)))
                    (define ?r (first h_r))
                    (define om (find_output_machine))
                    (t_carry_to_machine ?r ?p om)
                    (release (second h_r)))))
)
