(begin
    (set-config-platform
        --path /home/jeremy/godot/simulation-factory-godot/simu
        --scenario /home/jeremy/godot/simulation-factory-godot/simu/scenarios/new_scenario_multirobots.json
        --environment /home/jeremy/godot/simulation-factory-godot/simu/environments/env_6_machines.json
        --jobshop /home/jeremy/godot/simulation-factory-godot/simu/jobshop/instances/j02.txt
        ;--robot_controller teleport
        --robot_controller pf
        --time_scale 4
    )
    (start)
    (sleep 0.5)
    ;(plan-task t_jobshop)
    (plan-task t_carry_to_machine robot0 package0 machine0)
    ;(plan-task robot_move robot1 tile_9_11)
    )