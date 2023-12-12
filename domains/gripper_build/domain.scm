(begin
    (define gripper-build-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_build"))
    (set-current-dir gripper-build-path)
    (load base.scm)
    (set-current-dir gripper-build-path)
    (load om.scm)
)