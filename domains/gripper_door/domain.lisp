(begin
    (define gripper-door-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
    (set-current-dir gripper-door-path)
    (read "../gripper/base.lisp")
    (read base.lisp)
    (read om.lisp)
)