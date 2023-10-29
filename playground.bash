#!/bin/bash
source ompas_config_local.bash
#cargo run --bin ompas -- -d domains/unit_tests/parameter.lisp
#cargo run --bin ompas -- -d domains/unit_tests/assign_3.lisp
#cargo run --bin ompas -- -d domains/unit_tests/acquire.lisp
#cargo run --release --bin ompas -- -d ompas-gobot-sim/planning_domain/domain_plan_debug.lisp
cargo run --bin ompas-gobot-sim -- -d ompas-gobot-sim/planning_domain/domain_plan.lisp
# cargo run --release --bin ompas-gobot-sim -- -d ompas-gobot-sim/planning_domain/domain_plan_extended.lisp
#cargo run --bin debug_aries