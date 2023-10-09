(begin
    (set_log_level trace)

    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper/"))
    (read domain.lisp)
    (read problems/problem_1.lisp)
    (print (get-domain))

    (start)
    ;(activate_log log-ompas)
    (wait-task 0)
    (dump_trace)

    (exit 0)
)

