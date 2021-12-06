(begin
    (def-state-function at-robby '(?r room))
    (def-state-function at '(?b ball) '(?r room))
    (def-state-function free '(?g gripper))
    (def-state-function carry '(?o ball) '(?g gripper))
)