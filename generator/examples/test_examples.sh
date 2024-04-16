cargo run --example test_yaml
cargo run --example gripper_generator -- -b 3 -r 3 -t 1;
cargo run --example gripper_door_generator -- -b 3 -r 3 -t 1 -d 2 -e 3;
cargo run --example gripper_multi_generator -- -b 3 -r 3 -t 1 -d 2 -e 3 -a 2;
cargo run --example gripper_build_generator -- -b 3 -r 3 -t 1 -d 2 -e 3 -a 2 -o 2;
export GOBOT_SIM_PATH=$OMPAS_PATH/ompas-gobot-sim/gobot-sim/simu
cargo run --example jobshop_generator -- -j 3 -p 3 -l 3 -u 8
# Uncomment to test on godot
# godot3 --path $GOBOT_SIM_PATH --scenario /tmp/jobshop/problem_scenario.json
cargo run --example continuous_shop_generator -- -j 3 -p 4 -l 3 -u 8 -i 1 -a 3
# godot3 --path $GOBOT_SIM_PATH --scenario /tmp/continuous/problem_scenario.json





