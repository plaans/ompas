(begin
    (define gripper-build-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_build"))
    (set-current-dir gripper-build-path)
    (read "../gripper/base.lisp")
    (read "../gripper_door/base.lisp")
    (read "../gripper_multi/base.lisp")
    (read base.lisp)
    (read om.lisp)
)