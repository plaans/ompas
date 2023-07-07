(begin
    (define path (get-env-var "OMPAS_PATH"))
    (define gobot-sim-path (concatenate path "/ompas-gobot-sim/gobot-sim/simu/"))
    (set-config-platform
        --path gobot-sim-path
        --scenario (concatenate gobot-sim-path "scenarios/new_scenario.json")
        --environment (concatenate gobot-sim-path "environments/new_environment.json")
        --robot_controller pf
        --time_scale 4
    )
)