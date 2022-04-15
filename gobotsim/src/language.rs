//commands

pub const OPEN_COM: &str = "open-com-godot";
pub const START_GODOT: &str = "start-godot";
pub const LAUNCH_GODOT: &str = "launch-godot";
pub const EXEC_GODOT: &str = "exec-godot";

pub const GODOT_LAUNCH_PLATFORM: &str = "godot-launch-platform";

//State variables
//Lambda functions that will be added natively to the environment
//Depends on module state and function get-state.
//Robot

//Lambdas for robots.

//coordinates
pub const LAMBDA_ROBOT_COORDINATES: &str = "(define robot.coordinates (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote robot.coordinates) x))))";
pub const SF_ROBOT_COORDINATES: &str = "robot.coordinates";
pub const DOC_SF_COORDINATES: &str = "Return the coordinates (float float) of a robot";
pub const DOC_SF_ROBOT_COORDINATES_VERBOSE: &str = "Example: (robot.coordinates robot0)";

//battery
pub const LAMBDA_ROBOT_BATTERY: &str = "(define robot.battery (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.battery) x))))";
pub const SF_ROBOT_BATTERY: &str = "battery";
pub const DOC_SF_ROBOT_BATTERY: &str = "Return the battery level (float) in [0;1] of a robot.";
pub const DOC_SF_ROBOT_BATTERY_VERBOSE: &str = "Example: (robot.battery robot0)";

//rotation
pub const LAMBDA_ROBOT_ROTATION: &str = "(define robot.rotation (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.rotation) x))))";
pub const SF_ROBOT_ROTATION: &str = "robot.rotation";
pub const DOC_SF_ROBOT_ROTATION: &str = "Return the rotation value (float) of a robot.";
pub const DOC_SF_ROBOT_ROTATION_VERBOSE: &str = "Example: (robot.rotation robot0)";

//velocity
pub const LAMBDA_ROBOT_VELOCITY: &str = "(define robot.velocity (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.velocity) x))))";
pub const SF_ROBOT_VELOCITY: &str = "robot.velocity";
pub const DOC_SF_ROBOT_VELOCITY: &str =
    "Return the velocity value (float float) in x and y of a robot.";
pub const DOC_SF_ROBOT_VELOCITY_VERBOSE: &str = "Example: (robot.velocity robot0)";

//rotation speed
pub const LAMBDA_ROBOT_ROTATION_SPEED: &str = "(define robot.rotation_speed (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.rotation_speed) x))))";
pub const SF_ROBOT_ROTATION_SPEED: &str = "robot.rotation_speed";
pub const DOC_SF_ROBOT_ROTATION_SPEED: &str = "Return the rotation speed value (float) of a robot.";
pub const DOC_SF_ROBOT_ROTATION_SPEED_VERBOSE: &str = "Example: (robot.rotation_speed robot0)";

//in station
pub const LAMBDA_ROBOT_IN_STATION: &str = "(define robot.in_station (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.in_station) x))))";
pub const SF_ROBOT_IN_STATION: &str = "robot.in_station";
pub const DOC_SF_ROBOT_IN_STATION: &str = "Return true if a robot is in a station.";
pub const DOC_SF_ROBOT_IN_STATION_VERBOSE: &str = "Example: (in_station robot0)";

//in interact areas
pub const LAMBDA_ROBOT_IN_INTERACT_AREAS: &str = "(define robot.in_interact_areas (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote robot.in_interact_areas) x))))";
pub const SF_ROBOT_IN_INTERACT_AREAS: &str = "robot.in_interact_areas";
pub const DOC_SF_ROBOT_IN_INTERACT_AREAS: &str = "Return true if a robot is in an interact zone.";
pub const DOC_SF_ROBOT_IN_INTERACT_AREAS_VERBOSE: &str =
    "Example: (robot.in_interact_areas robot0)";

//Lambdas for machines.

//coordinates
pub const LAMBDA_MACHINE_COORDINATES: &str = "(define machine.coordinates (lambda (x)\
                                                          (get-map (get-state static) (list (quote machine.coordinates) x))))";
pub const SF_MACHINE_COORDINATES: &str = "machine.coordinates";
pub const DOC_SF_MACHINE_COORDINATES: &str = "todo!";

//coordinates tile
pub const LAMBDA_MACHINE_COORDINATES_TILE: &str = "(define machine.coordinates_tile (lambda (x)\
                                                          (get-map (get-state static) (list (quote machine.coordinates_tile) x))))";
pub const SF_MACHINE_COORDINATES_TILE: &str = "machine.coordinates_tile";
pub const DOC_SF_MACHINE_COORDINATES_TILE: &str = "todo!";

//input belt
pub const LAMBDA_MACHINE_INPUT_BELT: &str = "(define machine.input_belt (lambda (x)\
                                                        (get-map (get-state static) (list (quote machine.input_belt) x))))";
pub const SF_MACHINE_INPUT_BELT: &str = "machine.input_belt";
pub const DOC_SF_MACHINE_INPUT_BELT: &str =
    "Return the name (symbol) of the input belt of a machine.";
pub const DOC_SF_MACHINE_INPUT_BELT_VERBOSE: &str = "Example: (machine.input_belt machine0)";

//output belt
pub const LAMBDA_MACHINE_OUTPUT_BELT: &str = "(define machine.output_belt (lambda (x)\
                                                        (get-map (get-state static) (list (quote machine.output_belt) x))))";
pub const SF_MACHINE_OUTPUT_BELT: &str = "machine.output_belt";
pub const DOC_SF_MACHINE_OUTPUT_BELT: &str =
    "Return the name (symbol) of the output belt of a machine.";
pub const DOC_SF_MACHINE_OUTPUT_BELT_VERBOSE: &str = "Example: (machine.output_belt machine0)";

//processes
pub const LAMBDA_MACHINE_PROCESSES: &str = "(define machine.processes_list (lambda (x)\
                                                        (get-map (get-state static) (list (quote machine.processes_list) x))))";
pub const SF_MACHINE_PROCESSES: &str = "machine.processes_list";
pub const DOC_SF_MACHINE_PROCESSES: &str = "Return the list of processes (id0 id1 ...) of a machine, or return the list of pair (process_id, duration) for a package.";
pub const DOC_SF_MACHINE_PROCESSES_VERBOSE: &str = "Example:\n
                                        \t- for a machine: (processes machine0) \n\
                                        \t- for a package: (processes package0)";

//progress rate
pub const LAMBDA_MACHINE_PROGRESS_RATE: &str =  "(define machine.progress_rate (lambda (x)\
                                                        (get-map (get-state dynamic) (list (quote machine.progress_rate) x))))";
pub const SF_MACHINE_PROGRESS_RATE: &str = "machine.progress_rate";
pub const DOC_SF_MACHINE_PROGRESS_RATE: &str = "Return the progress rate (float) in [0;1] of a machine. If no task is in progress, the value is 0";
pub const DOC_SF_MACHINE_PROGRESS_RATE_VERBOSE: &str = "Example: (machine.progress_rate machine0)";

//Lambdas for packages.

//location
pub const LAMBDA_PACKAGE_LOCATION: &str = "(define package.location (lambda (x)\
                                                  (get-map (get-state dynamic) (list (quote package.location) x))))";
pub const SF_PACKAGE_LOCATION: &str = "package.location";
pub const DOC_SF_PACKAGE_LOCATION: &str = "Return the location (symbol) of a package.";
pub const DOC_SF_PACKAGE_LOCATION_VERBOSE: &str = "Example: (package.location package0)";

//processes list
pub const LAMBDA_PACKAGE_PROCESSES_LIST: &str = "(define package.processes_list (lambda (x)\
                                                  (get-map (get-state dynamic) (list (quote package.processes_list) x))))";
pub const SF_PACKAGE_PROCESSES_LIST: &str = "package.processes_list";
pub const DOC_SF_PACKAGE_PROCESSES_LIST: &str = "Return the location (symbol) of a package.";
pub const DOC_SF_PACKAGE_PROCESSES_LIST_VERBOSE: &str =
    "Example: (package.processes_list package0)";

//Lambdas for belts.

//belt type
pub const LAMBDA_BELT_BELT_TYPE: &str = "(define belt.belt_type (lambda (x)\
                                                    (get-map (get-state static) (list (quote belt.belt_type) x)))))";
pub const SF_BELT_BELT_TYPE: &str = "belt_type";
pub const DOC_SF_BELT_BELT_TYPE: &str =
    "Return the belt type (symbol) in {input, output} of a belt.";
pub const DOC_SF_BELT_BELT_TYPE_VERBOSE: &str = "Example: (belt_type belt0)";

//polygons
pub const LAMBDA_BELT_POLYGONS: &str = "(define belt.polygons (lambda (x)\
                                                    (get-map (get-state static) (list (quote belt.polygons) x)))))";
pub const SF_BELT_POLYGONS: &str = "belt.polygons";
pub const DOC_SF_BELT_POLYGONS: &str =
    "Return the coordinates of the polygon [(float float)] that represent the parking area";
pub const DOC_SF_BELT_POLYGONS_VERBOSE: &str = "Example: (belt.polygons parking_area0)";

//cells
pub const LAMBDA_BELT_CELLS: &str =
    "(define belt.cells (lambda (b) (get-map (get-state static) (list (quote belt.cells) b))))";
pub const SF_BELT_CELLS: &str = "belt.cells";
pub const DOC_SF_BELT_CELLS: &str = "todo!";

//interact areas
pub const LAMBDA_BELT_INTERACT_AREAS: &str = "(define belt.interact_areas (lambda (b) (get-map (get-state static) (list (quote belt.interact_areas) b))))";
pub const SF_BELT_INTERACT_AREAS: &str = "belt.interact_ares";
pub const DOC_SF_BELT_INTERACT_AREAS: &str = "todo!";

//packages list
pub const LAMBDA_BELT_PACKAGES_LIST: &str = "(define belt.packages_list (lambda (x)\
                                                (get-map (get-state dynamic) (list (quote belt.packages_list) x)))))";
pub const SF_BELT_PACKAGES_LIST: &str = "belt.packages_list";
pub const DOC_SF_BELT_PACKAGES_LIST: &str = "Return the package list [symbol] on a belt.";
pub const DOC_SF_BELT_PACKAGES_LIST_VERBOSE: &str = "Example: (belt.packages_list package0)";

//Lambdas for parking areas.

//polygons
pub const LAMBDA_PARKING_AREA_POLYGONS: &str = "(define parking_area.polygons (lambda (x)\
                                                                         (get-map (get-state static) (list (quote parking_area.polygons) x)))))";
pub const SF_PARKING_AREA_POLYGONS: &str = "parking_area.polygons";
pub const DOC_SF_PARKING_AREA_POLYGONS: &str = "todo!";

//cells
pub const LAMBDA_PARKING_AREA_CELLS: &str = "(define parking_area.cells (lambda (x)\
                                                                         (get-map (get-state static) (list (quote parking_area.cells) x)))))";
pub const SF_PARKING_AREA_CELLS: &str = "parking_area.cells";
pub const DOC_SF_PARKING_AREA_CELLS: &str = "todo!";

//Lambdas interact areas.

//polygons
pub const LAMBDA_INTERACT_AREA_POLYGONS: &str = "(define interact_area.polygons (lambda (x)\
                                                           (get-map (get-state static) (list (quote interact_area.polygons) x)))))";
pub const SF_INTERACT_AREA_POLYGONS: &str = "interact_area.polygons";
pub const DOC_SF_INTERACT_AREA_POLYGONS: &str = "todo!";

//cells
pub const LAMBDA_INTERACT_AREA_CELLS: &str = "(define interact_area.cells (lambda (x)\
                                                           (get-map (get-state static) (list (quote interact_area.cells) x)))))";
pub const SF_INTERACT_AREA_CELLS: &str = "interact_area.cells";
pub const DOC_SF_INTERACT_AREA_CELLS: &str = "todo!";

//belt
pub const LAMBDA_INTERACT_AREA_BELT: &str = "(define interact_area.belt (lambda (x)\
                                                           (get-map (get-state static) (list (quote interact_area.belt) x)))))";
pub const SF_INTERACT_AREA_BELT: &str = "interact_area.belt";
pub const DOC_SF_INTERACT_AREA_BELT: &str = "todo!";

//Lambdas for actions.

//rotation
pub const LAMBDA_DO_ROTATION: &str =
    "(define robot.do_rotation (lambda (r a w) (exec-godot do_rotation r a w)))";
pub const ACTION_DO_ROTATION: &str = "robot.do_rotation";
pub const DOC_ACTION_DO_ROTATION: &str = "todo!";

//pick
pub const LAMBDA_PICK: &str = "(define robot.pick (lambda (r a w) (exec-godot pick r a w)))";
pub const ACTION_PICK: &str = "robot.pick";
pub const DOC_ACTION_PICK: &str = "todo!";

//place
pub const LAMBDA_PLACE: &str = "(define robot.place (lambda (r a w) (exec-godot place r a w)))";
pub const ACTION_PLACE: &str = "robot.place";
pub const DOC_ACTION_PLACE: &str = "todo!";

//navigate_to
pub const LAMBDA_NAVIGATE_TO: &str =
    "(define robot.navigate_to (lambda (r a w) (exec-godot navigate_to r a w)))";
pub const ACTION_NAVIGATE_TO: &str = "robot.navigate_to";
pub const DOC_ACTION_NAVIGATE_TO: &str = "todo!";

//Constants

//Documentation
pub const DOC_MOD_GODOT: &str = "Module to use the simulator developped in godot. It add functions and lambda for the state variables";
pub const DOC_MOD_GODOT_VERBOSE: &str = "functions:\n\
                                     \t- open-com-godot\n\
                                     \t- launch-godot (not implemented yet)\n\
                                     \t- exec-godot\n\
                                     \t- get-state\n\n\
                                     lambdas for the state functions:\n\
                                     \t- for a robot: coordinates, battery, rotation, velocity, rotation_speed, in_station, in_interact\n\
                                     \t- for a machine: coordinates, input_belt, output_belt, processes, progress_rate\n\
                                     \t- for a package: location, processes\n\
                                     \t- for a belt: coordinates, belt_type, polygons, packages_list\n\
                                     \t- for a parking are: polygon";

pub const DOC_OPEN_COM: &str = "Connect via TCP to the simulator.";
pub const DOC_OPEN_COM_VERBOSE: &str = "Default socket address is 127.0.0.1:10000, but you can provide the IP address and the port that you want.";
pub const DOC_LAUNCH_GODOT: &str = "Not yet implemented";
pub const DOC_EXEC_GODOT: &str = "Send a command to the simulator";
pub const DOC_EXEC_GODOT_VERBOSE: &str = "Available commands:\n\
                                       \t- Navigate to : [navigate_to', robot_name, destination_x, destination_y] \n\
                                       \t- Pick : ['pickup', robot_name] \n\
                                       \t- Place : ['place', robot_name] \n\
                                       \t- Rotation : ['do_rotation', robot_name, angle, speed] \n\n\
                                       Example: (exec-godot navigate_to robot0 2 3)";

//functions
//const SET_STATE: &str = "set-state";
pub const GET_STATE: &str = "get-state";
//const UPDATE_STATE: &str = "update-state";

//Documentation

pub const DOC_GET_STATE: &str = "Return the current state.";
pub const DOC_GET_STATE_VERBOSE: &str = "Takes an optional argument: {static, dynamic}";
