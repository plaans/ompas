(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (load planning_domain/base.scm)
    (load planning_domain/lambdas.scm)
    (load planning_domain/plan_model.scm)
    ;(load planning_domain/problems/j06.scm)
    (load planning_domain/problems/j01.scm)

    ;(load planning_domain/problems/p1.scm)
    ;(load planning_domain/problems/p1_debug.scm)
    ;(plan t_jobshop)

    ;(load planning_domain/problems/ft06.scm)
    ;(start)
    ;(sleep 1)
    ;(plan t_jobshop)

    ;(start)
    (start-with-planner false)
    ;(start-acting-tree-display)

    (wait-task (exec-task t_jobshop))
    (sleep 2)
    (export-report gobot-sim.jobshop)
    (exit 0)
)
