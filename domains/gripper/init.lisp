(begin
    (read domains/gripper/domain.lisp)
    ;(read domains/gripper/actions_test.lisp)
    (read domains/gripper/problems/problem_1.lisp)
    ;(set-select aries)
    (cp.start false)
    ;(sleep 0.5)
    (cp.new-task pick-and-drop b1 kitchen)
    ;(cp.plan)
    (cp.new-task pick-and-drop b2 kitchen)
    (cp.new-task pick-and-drop b3 kitchen)
)

