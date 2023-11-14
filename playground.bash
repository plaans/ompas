#!/bin/bash
source ompas_config_local.bash
#cargo run --bin ompas -- -d domains/unit_tests/parameter.lisp
#cargo run --bin ompas -- -d domains/unit_tests/assign_3.lisp
#cargo run --bin ompas -- -d domains/unit_tests/acquire.lisp
#cargo run --release --bin ompas -- -d ompas-gobot-sim/planning_domain/domain_plan_debug.lisp
export RUST_BACKTRACE=1
#cargo run --release --bin ompas-gobot-sim -- -d ompas-gobot-sim/planning_domain/domain_plan.lisp
# cargo run --release --bin ompas-gobot-sim -- -d ompas-gobot-sim/planning_domain/domain_plan_extended.lisp
#cargo run --bin debug_aries
cargo run --bin bench -- -c benchmark/config.yml
#cargo run --release --bin ompas -- -d draft.lisp
#cargo run --release --bin ompas -- -d ~/ompas_output/benchmark/gripper/problems/gripper_simple_0_random_satisfactory.lisp
#cargo run --release --bin ompas -- -d ~/ompas_output/benchmark/gripper_multi/problems/gripper-multi_medium_0_random_satisfactory.lisp