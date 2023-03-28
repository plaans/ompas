(begin
    
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
                (define h2 (acquire ?r '(:priority 1)))
                (t_carry_to_machine ?r ?p ?m)
                (release h2)
                (t_process ?m ?p ?d))))
)