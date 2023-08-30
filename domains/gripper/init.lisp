(begin
    (read domains/gripper/domain.lisp)
    (read domains/gripper/problems/problem_1.lisp)
    (cp.start false)

    ; test with a single command
    ; (cp.new-task move living_room kitchen)
    ; (cp.set-start 1 0.5)
    ; (cp.new-event 5 at-robby no_place)
    ; (cp.set-end 1 10.4)

    ; test with a task
    (cp.new-task pick-and-drop b1 kitchen)
    (dump_trace)
    ;(cp.set-start 10 2.5)
    ;(cp.plan)
    ;(dump_trace)
    (exit 0)
)

