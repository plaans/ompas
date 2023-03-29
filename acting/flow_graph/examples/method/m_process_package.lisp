(begin
    (define ?r (arbitrary (instances robot)))
    (define h1 (acquire ?m))
    (define h2 (acquire ?r))
    (exec-task 'carry ?r ?p ?m)
    (release h2)
    (exec-task 'process ?m ?p ?d)
)))