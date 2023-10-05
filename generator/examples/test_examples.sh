cargo run --example test_yaml
cargo run --example gripper_generator -- -b 3 -r 3 -t 1;
cargo run --example gripper_door_generator -- -b 3 -r 3 -t 1 -d 2 -e 3;
cargo run --example gripper_multi_generator -- -b 3 -r 3 -t 1 -d 2 -e 3 -a 2;
cargo run --example gripper_build_generator -- -b 3 -r 3 -t 1 -d 2 -e 3 -a 2 -o 2;
cargo run --example jobshop_generator -- -j 3 -l 3 -u 8


