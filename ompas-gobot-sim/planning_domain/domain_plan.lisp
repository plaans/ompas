(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model.lisp)
    ;(read planning_domain/problems/j06.lisp)
    ;(read planning_domain/problems/j01.lisp)

    (read planning_domain/problems/p1.lisp)
    (plan t_jobshop)

    ;(start-with-planner false)
    ;(start)
    ;(activate_log log-ompas)
    ;(wait-task (trigger-task t_jobshop))
    ;(wait-task (trigger-task t_process_on_machine package0 machine0 3))
    (stop)
    (exit 0)
)
