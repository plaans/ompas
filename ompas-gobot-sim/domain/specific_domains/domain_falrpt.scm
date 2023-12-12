(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain"))
    (load base.scm)
    (load lambdas.scm)
    (load falrpt.scm)
)