(begin
    (load (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain/om.scm"))

    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
        (:pre-conditions true)
        (:score 0)
        (:body
            (do
                (define ?r (arbitrary (instances robot) rand-element))
                (define h1 (acquire ?m))
                (define h2 (acquire ?r))
                (t_carry_to_machine ?r ?p ?m)
                (release h2)
                (t_process ?m ?p ?d)
                )))
    (def-task t_output_package (:params (?p package)))
    (def-method m_output_package
        (:task t_output_package)
        (:params (?p package))
        (:body
            (do
                (define ?r (arbitrary (instances robot)))
                (define h_r (acquire ?r))
                (define om (find_output_machine))
                (t_carry_to_machine ?r ?p om)
                (release h_r))))


)