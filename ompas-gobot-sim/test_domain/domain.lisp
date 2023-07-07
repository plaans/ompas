(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim"))
    (read "test_domain/base.lisp")
    (read "test_domain/jobshop.lisp")
    (read "test_domain/greedy.lisp")
    (read "test_domain/basic.lisp"))