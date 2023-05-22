(set-config-platform
    --path gobot-sim/simu
    --scenario scenarios/new_scenario_multirobots.json
    --environment environments/env_6_machines.json
    --jobshop simu/jobshop/instances/j06.txt
    ;--robot_controller teleport
    --robot_controller pf
    --time_scale 4
)