(begin
    (set_log_level trace)
    
    (define ompas_path (get-env-var "OMPAS_PATH"))
    (define gripper_path (concatenate ompas_path "/domains/gripper/"))
    (read (concatenate gripper_path "domain.lisp"))
    (read (concatenate gripper_path "problems/problem_1.lisp"))

    (start)
    ;(activate_log log-ompas)
    (wait-task 0)
    (dump_trace)

    (exit 0)
)

