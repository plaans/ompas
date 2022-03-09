(begin
    (def-state-function at-robby '(?r room))
    (def-state-function at '(?b ball) '(?r room))
    (def-state-function carry '(?g gripper) '(?b ball))
)