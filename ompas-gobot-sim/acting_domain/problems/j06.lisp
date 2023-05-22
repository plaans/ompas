(set-config-platform
    --path gobot-sim/simu
    --scenario gobot-sim/simu/scenarios/new_scenario_multirobots.json
    --environment gobot-sim/simu/environments/env_6_machines.json
    --jobshop gobot-sim/simu/jobshop/instances/j06.txt
    ;--robot_controller teleport
    --robot_controller pf
    --time_scale 4
)