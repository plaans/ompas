(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain"))
    (read base.lisp)
    (read lambdas.lisp)
    (read fa.lisp)
)