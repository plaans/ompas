(begin
    (define gripper-multi-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_multi"))
    (set-current-dir gripper-multi-path)
    (read base.lisp)
    (set-current-dir gripper-multi-path)
    (read om.lisp)
)