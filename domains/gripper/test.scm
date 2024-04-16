(begin
    (set_log_level trace)

    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper/"))
    (load domain.scm)
    (load problems/problem_1.scm)
    (print (get-domain))

    (start)
    ;(activate_log log-ompas)
    (wait-task 0)
    (dump_trace)

    (exit 0)
)

