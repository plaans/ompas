(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain"))
    (load base.scm)
    (load lambdas.scm)
    ;(load plan_model.scm)
    (load om.scm))