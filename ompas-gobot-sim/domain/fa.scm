(begin
    (load (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain/om.scm"))

    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
        (:body 
            (do
                (define h_m (acquire ?m))
                (define h_r (acquire-in-list (instances robot)))
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
