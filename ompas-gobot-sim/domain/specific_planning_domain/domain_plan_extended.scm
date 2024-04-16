(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (load planning_domain/base.scm)
    (load planning_domain/lambdas.scm)
    (load planning_domain/plan_model_extended.scm)
    ;(load planning_domain/problems/j06.scm)
    ;(load planning_domain/problems/p1.scm)
    (load planning_domain/problems/j02.scm)

    ;start-up the simulator
    (start)
    (sleep 0.5)

    (plan t_jobshop)
    ;(plan-task-opt t_jobshop)
    ;(plan-task t_process machine0 package0 3)
    ;(plan-task t_carry_to_machine robot0 package0 machine0)
    ;(plan-task robot_move robot0 belt0)
    ;(plan-task robot_move robot0 belt12)
    (exit 0)
)
