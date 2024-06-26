#!/bin/bash
# Environment variables of the OMPAS project
SOURCE=${BASH_SOURCE[0]}
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPT_DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

echo "Script directory: $SCRIPT_DIR"

# choice of how resources are encoded in the planner
# - assignment: default encoding using linear constraints, conditions and effects
# export OMPAS_RESOURCE_ENCODING=assignment
# - addition: new CSP encoding using increase and decrease effects operators
export OMPAS_RESOURCE_ENCODING=addition

# print the generates chronicles
export OMPAS_CHRONICLE_DEBUG=full

# Frequency at which continuous planning will be updated
export OMPAS_DELIBERATION_FREQUENCY=1

# print the plan formatted for the acting tree
export OMPAS_PLAN_OUTPUT=true

# print the continuous planning deliberation phases
export OMPAS_DEBUG_CONTINUOUS_PLANNING=true

# activate the debug of OMPAS
export OMPAS_DEBUG=false

# path to the ompas project
export OMPAS_PATH=$SCRIPT_DIR

# activate the logs of OMPAS
export OMPAS_LOG=false

# Environment variables for scheme
export SOMPAS_DEBUG=false

export ARIES_PRINT_PLANNER_OUTPUT=true
export ARIES_LCP_TIME_SCALE=1000
export ARIES_PRINT_INITIAL_PROPAGATION=true
export ARIES_LCP_RELAXED_TEMPORAL_CONSTRAINT_TASK_METHOD=true;