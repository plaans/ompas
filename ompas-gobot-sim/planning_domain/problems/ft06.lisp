(begin
    (define path (get-env-var "OMPAS_PATH"))
    (define gobot-sim-path (concatenate path "ompas-gobot-sim/gobot-sim/simu/"))
    (set-config-platform
        --path gobot-sim-path
        --scenario (concatenate gobot-sim-path "scenarios/new_scenario_multirobots.json")
        --environment (concatenate gobot-sim-path "environments/env_6_machines.json")
        --jobshop (concatenate gobot-sim-path "jobshop/instances/ft06.txt")
        ;--robot_controller teleport
        --robot_controller pf
        --time_scale 4
    )
    (start)
    (sleep 0.5)
    (plan-task t_jobshop)
    ;(plan-task-opt t_jobshop)
)