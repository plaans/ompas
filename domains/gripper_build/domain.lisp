(begin
    (define gripper-build-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_build"))
    (set-current-dir gripper-build-path)
    (read base.lisp)
    (set-current-dir gripper-build-path)
    (read om.lisp)
)