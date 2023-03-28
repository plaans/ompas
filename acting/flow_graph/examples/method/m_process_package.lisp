(begin
    (define ?r (arbitrary (instances robot)))
    (define h1 (acquire ?m))
    (define h2 (acquire ?r))
    (exec-task 't_carry_to_machine ?r ?p ?m)
    (release h2)
    (exec-task 't_process ?m ?p ?d)
)))