(begin
    (read domains/gripper/domain.lisp)
    (read domains/gripper/problems/problem_1.lisp)
    (cp.start false)
    (cp.new-task pick-and-drop b1 kitchen)
    (cp.set-start 10 2.5)
    ;(sleep 0.001)
    (cp.plan)
    ;(dump_trace)
    (exit 0)
)

