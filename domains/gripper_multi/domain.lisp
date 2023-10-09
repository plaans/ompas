(begin
    (define gripper-multi-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_multi"))
    (set-current-dir gripper-multi-path)
    (read "../gripper/base.lisp")
    (read "../gripper_door/base.lisp")
    (read base.lisp)
    (read om.lisp)
)