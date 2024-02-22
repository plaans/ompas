(begin
    (set-current-dir (concatenate (get-env-var "OMPAS_PATH") "/ompas-gobot-sim/domain"))
    (load base.scm)
    (load lambdas.scm)
    (load om.scm)
    (load random.scm)
    ;(load fa.scm)
    (def-init (sleep 5))
    (set-log-level debug) ;setting log-level
    (set-select random) ; define the algorithm of select
    (set-deliberation-reactivity 4) ; define the reactivity of the deliberation
    (set-continuous-planning optimality) ; define the continuous planning mode: none, satisfactory, optimality
    (set-planner-reactivity 4)
    (set-pre-compute-models false)
    ;problem
    (load problems/continuous-shop_medium_0.scm)
    (start)
)