(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    ;(read planning_domain/plan_model.lisp)
    (read planning_domain/plan_model_debug.lisp)
    ;(read planning_domain/problems/p1_debug.lisp)
    ;(read planning_domain/problems/p2_debug.lisp)
    (read planning_domain/problems/p1.lisp)

    (new-timed-goal-task 1 t_jobshop)
    (plan)
    (exit 0)
)