#!/bin/bash
source ompas_config_local.bash
export RUST_BACKTRACE=1
#cargo run --bin debug_aries
#cargo run --bin bench -- -c benchmark/config.yml
#cargo run --release --bin ompas -- -d draft.lisp
#cargo run --release --bin ompas -- -d ~/ompas_output/benchmark/gripper/problems/gripper_simple_0_random_satisfactory.lisp
#cargo run --release --bin ompas -- -d ~/ompas_output/benchmark/gripper_build/problems/gripper-build_simple_0_random_satisfactory.lisp
#cargo run --bin bench -- -c benchmark/config.yml
cargo run --release --bin ompas -- -d ~/ompas_output/benchmark/gripper/problems/gripper_simple_4_random_satisfactory.lisp