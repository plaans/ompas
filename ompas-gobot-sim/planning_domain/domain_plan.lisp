(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model.lisp)
    (read planning_domain/problems/j02.lisp)

    (start-with-planner false)
    ;(start)
    (activate_log log-ompas)
    (trigger-task t_jobshop)
)
