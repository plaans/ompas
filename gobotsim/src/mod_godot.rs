#![allow(dead_code)]

use crate::language::*;
use crate::rae_interface::PlatformGodot;
use ompas_rae_scheme::rae_exec::RAEInterface;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::rae_state::RAEState;
use ompas_rae_structs::rae_state::*;
use sompas_macros::*;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::typelvalue::KindLValue;
use sompas_structs::{lerror, wrong_type};
use std::sync::Arc;
use tokio::sync::RwLock;
/*
LANGUAGE
*/

const MOD_GODOT: &str = "mod-godot";

#[derive(Default, Clone)]
pub struct SocketInfo {
    pub addr: String,
    pub port: usize,
}

pub struct CtxGodot {
    state: RAEState,
    agenda: Agenda,
    platform: Arc<RwLock<PlatformGodot>>,
}

impl Default for CtxGodot {
    fn default() -> Self {
        let state = RAEState::default();
        let agenda = Agenda::default();
        let platform = Arc::new(RwLock::new(PlatformGodot {
            socket_info: Default::default(),
            sender_socket: None,
            state: state.clone(),
            instance: Default::default(),
            agenda: agenda.clone(),
        }));
        Self {
            state,
            agenda,
            platform,
        }
    }
}

impl CtxGodot {
    pub fn get_state(&self) -> RAEState {
        self.state.clone()
    }
}

impl IntoModule for CtxGodot {
    fn into_module(self) -> Module {
        let raw_lisp = vec![
            LAMBDA_ROBOT_BATTERY,
            LAMBDA_ROBOT_COORDINATES,
            LAMBDA_ROBOT_IN_INTERACT_AREAS,
            LAMBDA_ROBOT_IN_STATION,
            LAMBDA_ROBOT_ROTATION,
            LAMBDA_ROBOT_ROTATION_SPEED,
            LAMBDA_ROBOT_VELOCITY,
            LAMBDA_BELT_BELT_TYPE,
            LAMBDA_BELT_PACKAGES_LIST,
            LAMBDA_BELT_POLYGONS,
            LAMBDA_BELT_INTERACT_AREAS,
            LAMBDA_BELT_CELLS,
            LAMBDA_PACKAGE_LOCATION,
            LAMBDA_MACHINE_PROGRESS_RATE,
            LAMBDA_MACHINE_PROCESSES,
            LAMBDA_MACHINE_OUTPUT_BELT,
            LAMBDA_MACHINE_INPUT_BELT,
            LAMBDA_MACHINE_COORDINATES,
            LAMBDA_MACHINE_COORDINATES_TILE,
            LAMBDA_PARKING_AREA_POLYGONS,
            LAMBDA_PARKING_AREA_CELLS,
            LAMBDA_PACKAGE_PROCESSES_LIST,
            LAMBDA_INTERACT_AREA_BELT,
            LAMBDA_INTERACT_AREA_CELLS,
            LAMBDA_INTERACT_AREA_POLYGONS,
            LAMBDA_DO_ROTATION,
            LAMBDA_NAVIGATE_TO,
            LAMBDA_PICK,
            LAMBDA_PLACE,
        ]
        .into();

        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp,
            label: MOD_GODOT.to_string(),
        };

        module.add_async_fn_prelude(OPEN_COM, open_com);
        module.add_async_fn_prelude(LAUNCH_GODOT, launch_godot);
        module.add_async_fn_prelude(START_GODOT, start_godot);
        module.add_async_fn_prelude(EXEC_GODOT, exec_godot);
        module.add_async_fn_prelude(GET_STATE, get_state);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(MOD_GODOT, DOC_MOD_GODOT, DOC_MOD_GODOT_VERBOSE),
            LHelp::new_verbose(OPEN_COM, DOC_OPEN_COM, DOC_OPEN_COM_VERBOSE),
            LHelp::new(START_GODOT, DOC_LAUNCH_GODOT),
            LHelp::new_verbose(EXEC_GODOT, DOC_EXEC_GODOT, DOC_EXEC_GODOT_VERBOSE),
            //Add doc for state functions
            LHelp::new_verbose(
                SF_ROBOT_BATTERY,
                DOC_SF_ROBOT_BATTERY,
                DOC_SF_ROBOT_BATTERY_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_COORDINATES,
                DOC_SF_COORDINATES,
                DOC_SF_ROBOT_COORDINATES_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_IN_INTERACT_AREAS,
                DOC_SF_ROBOT_IN_INTERACT_AREAS,
                DOC_SF_ROBOT_IN_INTERACT_AREAS_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_IN_STATION,
                DOC_SF_ROBOT_IN_STATION,
                DOC_SF_ROBOT_IN_STATION_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_ROTATION,
                DOC_SF_ROBOT_ROTATION,
                DOC_SF_ROBOT_ROTATION_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_ROTATION_SPEED,
                DOC_SF_ROBOT_ROTATION_SPEED,
                DOC_SF_ROBOT_ROTATION_SPEED_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_ROBOT_VELOCITY,
                DOC_SF_ROBOT_VELOCITY,
                DOC_SF_ROBOT_VELOCITY_VERBOSE,
            ),
            LHelp::new(SF_MACHINE_COORDINATES, DOC_SF_MACHINE_COORDINATES),
            LHelp::new(SF_MACHINE_COORDINATES_TILE, DOC_SF_MACHINE_COORDINATES_TILE),
            LHelp::new_verbose(
                SF_MACHINE_INPUT_BELT,
                DOC_SF_MACHINE_INPUT_BELT,
                DOC_SF_MACHINE_INPUT_BELT_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_PACKAGE_LOCATION,
                DOC_SF_PACKAGE_LOCATION,
                DOC_SF_PACKAGE_LOCATION_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_BELT_PACKAGES_LIST,
                DOC_SF_BELT_PACKAGES_LIST,
                DOC_SF_BELT_PACKAGES_LIST_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_PACKAGE_PROCESSES_LIST,
                DOC_SF_PACKAGE_PROCESSES_LIST,
                DOC_SF_PACKAGE_PROCESSES_LIST_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_MACHINE_OUTPUT_BELT,
                DOC_SF_MACHINE_OUTPUT_BELT,
                DOC_SF_MACHINE_OUTPUT_BELT_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_MACHINE_PROCESSES,
                DOC_SF_MACHINE_PROCESSES,
                DOC_SF_MACHINE_PROCESSES_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_MACHINE_PROGRESS_RATE,
                DOC_SF_MACHINE_PROGRESS_RATE,
                DOC_SF_MACHINE_PROGRESS_RATE_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_BELT_POLYGONS,
                DOC_SF_BELT_POLYGONS,
                DOC_SF_BELT_POLYGONS_VERBOSE,
            ),
            LHelp::new_verbose(
                SF_BELT_BELT_TYPE,
                DOC_SF_BELT_BELT_TYPE,
                DOC_SF_BELT_BELT_TYPE_VERBOSE,
            ),
            LHelp::new(SF_BELT_CELLS, DOC_SF_BELT_CELLS),
            LHelp::new(SF_BELT_INTERACT_AREAS, DOC_SF_BELT_INTERACT_AREAS),
            LHelp::new(SF_INTERACT_AREA_BELT, DOC_SF_INTERACT_AREA_BELT),
            LHelp::new(SF_INTERACT_AREA_CELLS, DOC_SF_INTERACT_AREA_CELLS),
            LHelp::new(SF_INTERACT_AREA_POLYGONS, DOC_SF_INTERACT_AREA_POLYGONS),
            LHelp::new(SF_PARKING_AREA_POLYGONS, DOC_SF_PARKING_AREA_POLYGONS),
            LHelp::new(SF_PARKING_AREA_CELLS, DOC_SF_PARKING_AREA_CELLS),
            LHelp::new_verbose(GET_STATE, DOC_GET_STATE, DOC_GET_STATE_VERBOSE),
            LHelp::new(ACTION_DO_ROTATION, DOC_ACTION_DO_ROTATION),
            LHelp::new(ACTION_NAVIGATE_TO, DOC_ACTION_NAVIGATE_TO),
            LHelp::new(ACTION_PICK, DOC_ACTION_PICK),
            LHelp::new(ACTION_PLACE, DOC_ACTION_PLACE),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

/*
Functions
 */

/// Launch the godot process the simulation and opens the com
#[async_scheme_fn]
async fn launch_godot(env: &LEnv, args: &[LValue]) -> LResult {
    let env = env.clone();
    let ctx = env.get_context::<CtxGodot>(MOD_GODOT).unwrap();
    let mut platform = ctx.platform.write().await;
    let future = platform.launch_platform(args).await;
    future
}

/// Opens the tcp communication to receive state and status update and send commands.
#[async_scheme_fn]
async fn open_com(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxGodot>(MOD_GODOT).unwrap();
    ctx.platform.write().await.open_com(args).await
}

pub const DEFAULT_PATH_PROJECT_GODOT: &str = "/home/jeremy/godot/simulation-factory-godot/simu";

///Launch godot
#[async_scheme_fn]
async fn start_godot(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxGodot>(MOD_GODOT).unwrap();
    ctx.platform.write().await.start_platform(args).await
}
/// Commands available
///- Navigate to : ['navigate_to', robot_name, destination_x, destination_y]
///- Pick : ['pickup', robot_name]
///- Place : ['place', robot_name]
///- Rotation : ['do_rotation', robot_name, angle, speed]

/// Sends a command to godot
#[async_scheme_fn]
async fn exec_godot(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxGodot>(MOD_GODOT).unwrap();
    let (id, _) = ctx.agenda.add_action(args.into(), 0).await;
    ctx.platform.read().await.exec_command(args, id).await
}

/// Returns the whole state if no args and a particular entry corresponding to the arg.
#[async_scheme_fn]
async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxGodot>(MOD_GODOT).unwrap();
    let _type = match args.len() {
        0 => None,
        1 => match &args[0] {
            LValue::Symbol(s) => match s.as_str() {
                KEY_STATIC => Some(StateType::Static),
                KEY_DYNAMIC => Some(StateType::Dynamic),
                _ => {
                    let result1 = Err(lerror!(
                        "PlatformGodot::get_state",
                        format!("Expected keywords {} or {}", KEY_STATIC, KEY_DYNAMIC)
                    ));
                    return result1;
                }
            },
            lv => {
                return Err(wrong_type!(
                    "PlatformGodot::get_state",
                    &lv,
                    KindLValue::Symbol
                ))
            }
        },
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                "PlatformGodot::get_state",
                args,
                0..1,
            ))
        }
    };

    //println!("state type: {:?}", _type);

    //let handle = tokio::runtime::Handle::current();
    //let state = self.state.clone();

    let result = ctx.state.get_state(_type).await;
    Ok(result.into_map())
}
