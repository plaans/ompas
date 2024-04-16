(begin
    (def-state-function test (:result boolean))
    (new-event test true '(10 12))
    (new-event test false '(20 22))
    (new-goal test true)
    (print (get-goals-events))
    (remove-task 0)
    (print (get-goals-events))
)