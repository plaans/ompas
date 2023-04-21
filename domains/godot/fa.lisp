(begin
    (def-task t_process_on_machine (:params (?p package) (?m machine) (?d int)))
    (def-method m_process_on_machine
        (:task t_process_on_machine)
        (:params (?p package) (?m machine) (?d int))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define h_m (acquire ?m))
                (define h_r (acquire-in-list (instances robot)))
                (define ?r (first h_r))
                (t_carry_to_machine ?r ?p ?m)
                (release (second h_r))
                (t_process ?m ?p ?d)
                (wait-for `(!= (package.location ,?p) ,?m))
                )))

)
