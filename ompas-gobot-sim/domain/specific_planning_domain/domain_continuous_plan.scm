(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (load planning_domain/base.scm)
    (load planning_domain/lambdas.scm)
    (load planning_domain/plan_model.scm)
    ;(load planning_domain/problems/j02.scm)
    ;start simulator
    ;(start)
    ;(sleep 0.5)
    (load planning_domain/problems/p1_debug.scm)

    ;test continuous planning capabilities
    ;start the continuous planner
    (cp.start false)
    ;adds a new task to address
    (cp.new-task t_jobshop)
    ;replan with the same structure
    (cp.plan)
)
