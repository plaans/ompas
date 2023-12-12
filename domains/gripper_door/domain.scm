(begin
    (define gripper-door-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
    (set-current-dir gripper-door-path)
    (load base.scm)
    (set-current-dir gripper-door-path)
    (load om.scm)
)