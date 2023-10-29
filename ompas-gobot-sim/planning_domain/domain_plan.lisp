(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model.lisp)
    ;(read planning_domain/problems/j06.lisp)
    (read planning_domain/problems/j01.lisp)

    ;(read planning_domain/problems/p1.lisp)
    ;(read planning_domain/problems/p1_debug.lisp)
    ;(plan t_jobshop)

    ;(read planning_domain/problems/ft06.lisp)
    ;(start)
    ;(sleep 1)
    ;(plan t_jobshop)

    (start-with-planner true)
    (start-acting-tree-display)

    (wait-task (exec-task t_jobshop))
    (exit 0)
    ;(exec-task t_jobshop)

)
