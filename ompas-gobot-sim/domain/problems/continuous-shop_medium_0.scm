(begin
    (define ompas_path (get-env-var "OMPAS_PATH"))
    (define gobot-sim-path (concatenate ompas_path "/ompas-gobot-sim/gobot-sim/simu/"))
    (set-config-platform
         --path gobot-sim-path
        --scenario (concatenate ompas_path "ompas-gobot-sim/domain/problems/continuous-shop_medium_0_scenario.json")
        --robot_controller pf
        --time_scale 1))