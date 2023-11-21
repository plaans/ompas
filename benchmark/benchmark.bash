#!/bin/bash
#source ../ompas_config_local.bash
export RUST_BACKTRACE=0
#cargo run --bin generator -- -c generator/config.yml
cargo run --bin bench -- -c config.yml