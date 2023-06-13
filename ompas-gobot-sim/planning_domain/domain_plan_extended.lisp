(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model_extended.lisp)
    (read planning_domain/problems/j06.lisp)
    ;(read planning_domain/problems/p1.lisp)
    ;(read planning_domain/problems/j02.lisp)
)