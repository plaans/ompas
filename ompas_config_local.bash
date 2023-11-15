# Environment variables of the OMPAS project

# SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
#echo "Script directory: $SCRIPT_DIR"

# choice of how resources are encoded in the planner
# - assignment: default encoding using linear constraints, conditions and effects
#export OMPAS_RESOURCE_ENCODING=assignment
# - addition: new CSP encoding using increase and decrease effects operators
export OMPAS_RESOURCE_ENCODING=addition

# print the generates chronicles
export OMPAS_CHRONICLE_DEBUG=off

export OMPAS_PRE_COMPUTE_MODELS=true

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

export OMPAS_WORKING_DIR=/home/jeremy/ompas_output

# activate the logs of OMPAS
export OMPAS_LOG=false

# Environment variables for scheme
export SOMPAS_DEBUG=false

export ARIES_PRINT_PLANNER_OUTPUT=true
export ARIES_LCP_TIME_SCALE=1000
export ARIES_PRINT_INITIAL_PROPAGATION=false
export ARIES_LCP_RELAXED_TEMPORAL_CONSTRAINT_TASK_METHOD=true;
export ARIES_RESOURCE_LA_USE_ASSIGN_END_TIMEPOINT=true;
export ARIES_PRINT_MODEL=false
export ARIES_PRINT_RAW_MODEL=false
