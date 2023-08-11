(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/"))
    (read planning_domain/base.lisp)
    (read planning_domain/lambdas.lisp)
    (read planning_domain/plan_model.lisp)
    ;(read planning_domain/problems/p1.lisp)
    (read planning_domain/problems/j02.lisp)
    ;(read planning_domain/problems/j06.lisp)
    ;(read planning_domain/problems/ft06.lisp)

    ;start-up the simulator
    (start)
    (sleep 0.5)

    ;for test purpose
    (new-goal-task t_jobshop)
    (plan)

    ;(def-state-function test (:result boolean))
    ;(new-event test true '(10 12))
    ;(new-event test false '(20 22))
    ;(new-goal test true)
    ;(print (get-goals-events))
    ;(remove-task 0)
    ;(print (get-goals-events))

    ;(plan-in-ompas t_jobshop)
    ;(exit 0)
    ;(plan-task t_process machine0 package0 3)
    ;(plan-task t_carry_to_machine robot0 package0 machine0)
    ;(plan-task robot_move robot0 belt0)
    ;(plan-task robot_move robot0 belt12)
)
