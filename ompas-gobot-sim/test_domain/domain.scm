(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim"))
    (load "test_domain/base.scm")
    (load "test_domain/jobshop.scm")
    (load "test_domain/greedy.scm")
    (load "test_domain/j06.scm"))