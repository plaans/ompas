# Environment variables of the OMPAS project

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# print the generates chronicles
export OMPAS_CHRONICLE_DEBUG=full

# print the plan formatted for the acting tree
export OMPAS_PLAN_OUTPUT=false

# activate the debug of OMPAS
export OMPAS_DEBUG=false

# path to the ompas project
export OMPAS_PATH=${SCRIPT_DIR}

# activate the logs of OMPAS
export OMPAS_LOG=false

# Environment variables for scheme
export SOMPAS_DEBUG=false

export ARIES_LCP_TIME_SCALE=1000

export ARIES_PRINT_INITIAL_PROPAGATION=true

# Environemnt variables for ompas-gobot-sim
#export GODOT3_PATH=$HOME/godot3
#export GODOT3_HEADLESS_PATH=$HOME/godot3-headless

# path to craft-bots main directory
# export CRAFT_BOTS_PATH=$OMPAS_PATH/craft-bots