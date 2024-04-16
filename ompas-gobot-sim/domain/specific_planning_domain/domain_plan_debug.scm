(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (load planning_domain/base.scm)
    (load planning_domain/lambdas.scm)
    ;(load planning_domain/plan_model.scm)
    (load planning_domain/plan_model_debug.scm)
    ;(load planning_domain/problems/p1_debug.scm)
    ;(load planning_domain/problems/p2_debug.scm)
    (load planning_domain/problems/p1.scm)

    (new-timed-goal-task 1 t_jobshop)
    (plan)
    (exit 0)
)