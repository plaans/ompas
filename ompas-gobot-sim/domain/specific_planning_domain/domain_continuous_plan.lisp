(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model.lisp)
    ;(read planning_domain/problems/j02.lisp)
    ;start simulator
    ;(start)
    ;(sleep 0.5)
    (read planning_domain/problems/p1_debug.lisp)

    ;test continuous planning capabilities
    ;start the continuous planner
    (cp.start false)
    ;adds a new task to address
    (cp.new-task t_jobshop)
    ;replan with the same structure
    (cp.plan)
)
