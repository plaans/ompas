# Environment variables of the OMPAS project

# SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
# echo "Script directory: $SCRIPT_DIR"

# print the generates chronicles
export OMPAS_CHRONICLE_DEBUG=full

export OMPAS_DELIBERATION_FREQUENCY=1

# print the plan formatted for the acting tree
export OMPAS_PLAN_OUTPUT=true

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
export ARIES_PRINT_INITIAL_PROPAGATION=false
export ARIES_LCP_RELAXED_TEMPORAL_CONSTRAINT_TASK_METHOD=true;