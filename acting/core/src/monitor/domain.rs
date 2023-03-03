use crate::monitor::ModMonitor;
use ompas_language::monitor::domain::*;
use ompas_structs::acting_domain::command::Command;
use ompas_structs::acting_domain::method::Method;
use ompas_structs::acting_domain::parameters::{try_domain_from_lvalue, Parameters};
use ompas_structs::acting_domain::state_function::StateFunction;
use ompas_structs::acting_domain::task::Task;
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::conversion::context::ConversionContext;
use ompas_structs::execution::resource::{Capacity, ResourceManager};
use ompas_structs::state::partial_state::PartialState;
use ompas_structs::state::world_state::{StateType, WorldState, WorldStateSnapshot};
use ompas_structs::sym_table::domain::ref_type_lattice::RefTypeLattice;
use ompas_structs::sym_table::domain::Domain;
use sompas_core::modules::list::{car, cons, first};
use sompas_core::{eval, expand, get_root_env, parse};
use sompas_language::kind::*;
use sompas_language::predicate::*;
use sompas_macros::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::kindlvalue::KindLValue::{List, Symbol};
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::llambda::LLambda;
use sompas_structs::lmodule::{InitScheme, LModule};
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_n_args, wrong_type};
use std::convert::TryInto;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct ModDomain {
    state: WorldState,
    resource_manager: ResourceManager,
    empty_env: LEnv,
    domain_description: InitScheme,
    domain: Arc<RwLock<OMPASDomain>>,
}

impl ModDomain {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            state: monitor.acting_manager.state.clone(),
            resource_manager: monitor.acting_manager.resource_manager.clone(),
            empty_env: monitor.empty_env.clone(),
            domain_description: monitor.platform.domain().into(),
            domain: monitor.ompas_domain.clone(),
        }
    }

    pub fn get_empty_env(&self) -> LEnv {
        self.empty_env.clone()
    }

    pub async fn get_conversion_context(&self) -> ConversionContext {
        let mut state: WorldStateSnapshot = self.state.get_snapshot().await;
        let snap_resource: WorldStateSnapshot = self.resource_manager.get_snapshot().await;
        state.absorb(snap_resource);
        let lattice = state.instance.lattice.get_lattice().await;
        let domain: OMPASDomain = self.domain.read().await.clone();
        let env_symbols: LEnvSymbols = self.domain.read().await.get_convert_env();
        let mut env = self.empty_env.clone();
        env.set_new_top_symbols(env_symbols);
        ConversionContext::new(domain, lattice, state, env)
    }
}

impl From<ModDomain> for LModule {
    fn from(m: ModDomain) -> Self {
        let prelude = m.domain_description.clone();
        let mut module = LModule::new(m, MOD_DOMAIN, DOC_MOD_DOMAIN);
        module.add_async_fn(
            GENERATE_TEST_TYPE_EXPR,
            generate_test_type_expr,
            DOC_GENERATE_TEST_TYPE_EXPR,
            false,
        );
        module.add_async_fn(
            ADD_STATE_FUNCTION,
            add_state_function,
            DOC_ADD_STATE_FUNCTION,
            false,
        );
        module.add_async_fn(
            ADD_STATIC_STATE_FUNCTION,
            add_static_state_function,
            DOC_ADD_STATIC_STATE_FUNCTION,
            false,
        );
        module.add_async_fn(ADD_COMMAND, add_command, DOC_ADD_COMMAND, false);
        module.add_async_fn(
            ADD_COMMAND_MODEL,
            add_command_model,
            DOC_ADD_COMMAND_MODEL,
            false,
        );
        module.add_async_fn(ADD_TASK, add_task, DOC_ADD_TASK, false);
        module.add_async_fn(ADD_TASK_MODEL, add_task_model, DOC_ADD_TASK_MODEL, false);
        module.add_async_fn(ADD_METHOD, add_method, DOC_ADD_METHOD, false);
        module.add_async_fn(ADD_LAMBDA, add_lambda, DOC_ADD_LAMBDA, false);
        module.add_async_fn(ADD_FACTS, add_facts, DOC_ADD_FACTS, false);
        module.add_async_fn(
            ADD_STATIC_FACTS,
            add_static_facts,
            DOC_ADD_STATIC_FACTS,
            false,
        );
        module.add_async_fn(ADD_TYPE, add_type, DOC_ADD_TYPE, false);
        module.add_async_fn(ADD_TYPES, add_types, DOC_ADD_TYPES, false);
        module.add_async_fn(ADD_OBJECT, add_object, DOC_ADD_OBJECT, false);
        module.add_async_fn(ADD_OBJECTS, add_objects, DOC_ADD_OBJECTS, false);
        module.add_async_fn(ADD_RESOURCE, add_resource, DOC_ADD_RESOURCE, false);
        module.add_async_fn(ADD_RESOURCES, add_resources, DOC_ADD_RESOUCES, false);

        //Macros
        module.add_macro(
            DEF_STATE_FUNCTION,
            MACRO_DEF_STATE_FUNCTION,
            (DOC_DEF_STATE_FUNCTION, DOC_DEF_STATE_FUNCTION_VERBOSE),
        );

        module.add_macro(
            DEF_STATIC_STATE_FUNCTION,
            MACRO_DEF_STATIC_STATE_FUNCTION,
            (DOC_DEF_STATE_FUNCTION, DOC_DEF_STATE_FUNCTION_VERBOSE),
        );
        module.add_macro(
            DEF_COMMAND,
            MACRO_DEF_COMMAND,
            (DOC_DEF_COMMAND, DOC_DEF_COMMAND_VERBOSE),
        );
        //todo: add macros to define command models
        module.add_macro(
            DEF_TASK,
            MACRO_DEF_TASK,
            (DOC_DEF_TASK, DOC_DEF_TASK_VERBOSE),
        );
        //todo: add macros to define task models
        module.add_macro(
            DEF_METHOD,
            MACRO_DEF_METHOD,
            (DOC_DEF_METHOD, DOC_DEF_METHOD_VERBOSE),
        );
        module.add_macro(
            DEF_LAMBDA,
            MACRO_DEF_LAMBDA,
            (DOC_DEF_LAMBDA, DOC_DEF_LAMBDA_VERBOSE),
        );
        module.add_macro(OM_MODEL, MACRO_OM_MODEL, DOC_OM_MODEL);
        module.add_macro(
            PDDL_MODEL,
            MACRO_PDDL_MODEL,
            (DOC_PDDL_MODEL, DOC_PDDL_MODEL_VERBOSE),
        );
        module.add_macro(
            DEF_COMMAND_OM_MODEL,
            MACRO_DEF_COMMAND_OM_MODEL,
            DOC_DEF_COMMAND_OM_MODEL,
        );
        module.add_macro(
            DEF_COMMAND_PDDL_MODEL,
            MACRO_DEF_COMMAND_PDDL_MODEL,
            DOC_DEF_COMMAND_PDDL_MODEL,
        );
        module.add_macro(
            DEF_TASK_OM_MODEL,
            MACRO_DEF_TASK_OM_MODEL,
            DOC_DEF_TASK_OM_MODEL,
        );
        module.add_macro(
            DEF_TASK_PDDL_MODEL,
            MACRO_DEF_TASK_PDDL_MODEL,
            DOC_DEF_TASK_PDDL_MODEL,
        );
        module.add_macro(
            DEF_FACTS,
            MACRO_DEF_FACTS,
            (DOC_DEF_FACTS, DOC_DEF_FACTS_VERBOSE),
        );

        module.add_macro(
            DEF_STATIC_FACTS,
            MACRO_DEF_STATIC_FACTS,
            DOC_DEF_STATIC_FACTS,
        );
        module.add_macro(
            DEF_TYPES,
            MACRO_DEF_TYPES,
            (DOC_DEF_TYPES, DOC_DEF_TYPES_VERBOSE),
        );
        module.add_macro(
            DEF_OBJECTS,
            MACRO_DEF_OBJECTS,
            (DOC_DEF_OBJECTS, DOC_DEF_OBJECTS_VERBOSE),
        );
        module.add_macro(DEF_RESOURCES, MACRO_DEF_RESOURCES, DOC_DEF_RESOURCES);

        module.add_prelude(prelude);
        module
    }
}

/// Takes as input a p_expr of the form ((p1 p1_type) ... (p_n pn_type))
#[async_scheme_fn]
pub async fn generate_test_type_expr(env: &LEnv, params: Vec<LValue>) -> LResult {
    if params.is_empty() {
        Ok(true.into())
    } else {
        let mut str = "(do ".to_string();
        for param in params {
            if let LValue::List(param) = &param {
                if param.len() == 2 {
                    if let LValue::Symbol(par) = &param[0] {
                        if let LValue::Symbol(tpe) = &param[1] {
                            let test = match tpe.as_str() {
                                LIST => {
                                    format!("({} {})", IS_LIST, par)
                                }
                                BOOL => format!("({} {})", IS_BOOL, par),
                                INT => format!("({} {})", IS_INT, par),
                                FLOAT => format!("({} {})", IS_FLOAT, par),
                                NUMBER => format!("({} {})", IS_NUMBER, par),
                                SYMBOL => format!("({} {})", IS_SYMBOL, par),
                                _ => format!("(instance {} {})", par, tpe),
                            };

                            str.push_str(format!("(check {})", test).as_str())
                        } else {
                            return Err(wrong_type!(
                                GENERATE_TEST_TYPE_EXPR,
                                &param[1],
                                KindLValue::Symbol
                            ));
                        }
                    } else {
                        return Err(wrong_type!(
                            GENERATE_TEST_TYPE_EXPR,
                            &param[0],
                            KindLValue::Symbol
                        ));
                    }
                } else {
                    return Err(wrong_n_args!(GENERATE_TEST_TYPE_EXPR, param, 2));
                }
            } else {
                return Err(wrong_type!(
                    GENERATE_TEST_TYPE_EXPR,
                    &param,
                    KindLValue::List
                ));
            }
        }
        str.push(')');

        let mut env = env.clone();
        expand(&parse(&str, &mut env).await?, true, &mut env).await
    }
}

/// Defines a state function in RAE environment.
#[async_scheme_fn]
pub async fn add_state_function(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;
    let mut new_env = ctx.get_empty_env();
    let label = map.get(&NAME.into()).unwrap();
    let params: Parameters = Parameters::try_from_lvalue(
        map.get(&PARAMETERS.into()).unwrap_or(&Default::default()),
        &lattice,
    )
    .await?;
    let result = car(
        env,
        &[map
            .get(&RESULT.into())
            .ok_or_else(|| {
                LRuntimeError::new(ADD_STATE_FUNCTION, format!("No a :result for {}", label))
            })?
            .clone()],
    )?;

    let result: Domain = try_domain_from_lvalue(&lattice, &result).await?;
    let expr = format!(
        "(lambda {}
                (read-state '{} {})))",
        params.get_params_as_lvalue(),
        label,
        {
            let mut str = String::new();
            for p in params.get_labels() {
                str.push_str(p.as_str());
                str.push(' ');
            }
            str
        }
    );
    let body = eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?;
    let state_function = StateFunction::new(label.to_string(), params, result, body);
    ctx.domain
        .write()
        .await
        .add_state_function(label.to_string(), state_function)?;
    Ok(())
}

/// Defines a state function in RAE environment.
#[async_scheme_fn]
pub async fn add_static_state_function(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;
    let mut new_env = ctx.get_empty_env();
    let label = map.get(&NAME.into()).unwrap();
    let params: Parameters = Parameters::try_from_lvalue(
        map.get(&PARAMETERS.into()).unwrap_or(&Default::default()),
        &lattice,
    )
    .await?;
    let result = car(
        env,
        &[map
            .get(&RESULT.into())
            .ok_or_else(|| {
                LRuntimeError::new(ADD_STATE_FUNCTION, format!("No a :result for {}", label))
            })?
            .clone()],
    )?;

    let result: Domain = try_domain_from_lvalue(&lattice, &result).await?;
    let expr = format!(
        "(lambda {}
                (read-static-state '{} {})))",
        params.get_params_as_lvalue(),
        label,
        {
            let mut str = String::new();
            for p in params.get_labels() {
                str.push_str(p.as_str());
                str.push(' ');
            }
            str
        }
    );
    let body = eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?;
    let state_function = StateFunction::new(label.to_string(), params, result, body);
    ctx.domain
        .write()
        .await
        .add_state_function(label.to_string(), state_function)?;
    Ok(())
}

async fn create_model(env: &LEnv, model: im::HashMap<LValue, LValue>) -> LResult {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;

    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;
    let env = &mut ctx.get_empty_env();
    let model_type: ModelType = model
        .get(&MODEL_TYPE.into())
        .unwrap()
        .to_string()
        .as_str()
        .try_into()?;
    let str = match model_type {
        ModelType::PDDL => {
            let params: Parameters = Parameters::try_from_lvalue(
                model
                    .get(&PARAMETERS.into())
                    .ok_or_else(|| LRuntimeError::new("create_model", "missing :params"))?,
                &lattice,
            )
            .await?;
            let conds = model
                .get(&PRE_CONDITIONS.into())
                .ok_or_else(|| LRuntimeError::new("create_model", "missing :pre-conditions"))?;
            let mut str_conds = "(do".to_string();
            if let LValue::List(conds) = conds {
                for cond in conds.iter() {
                    str_conds.push_str(format!("(check {})", cond).as_str());
                }
                str_conds.push(')');
            } else {
                return Err(LRuntimeError::default());
            }
            let effects = model
                .get(&EFFECTS.into())
                .ok_or_else(|| LRuntimeError::new("create_model", "missing :effects"))?;
            let effects = cons(env, &[LPrimitive::Do.into(), effects.clone()])?;
            let test =
                generate_test_type_expr(env, &[model.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            format!(
                "(lambda {} (do {} {} {}))",
                params.get_params_as_lvalue(),
                test,
                str_conds,
                effects
            )
        }
        ModelType::OM => {
            let params: Parameters = Parameters::try_from_lvalue(
                model
                    .get(&PARAMETERS.into())
                    .ok_or_else(|| LRuntimeError::new("create_model", "missing :params"))?,
                &lattice,
            )
            .await?;
            let body = car(env, &[model.get(&BODY.into()).unwrap().clone()])?;
            let test =
                generate_test_type_expr(env, &[model.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            format!(
                "(lambda {} (do {} {}))",
                params.get_params_as_lvalue(),
                test,
                body
            )
        }
    };

    eval(&parse(&str, env).await?, env, None).await
}

/// Defines an action in RAE environment.
#[async_scheme_fn]
pub async fn add_command(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            ADD_COMMAND,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;
    let mut env = ctx.get_empty_env();
    let mut command = Command::default();
    command.set_label(map.get(&NAME.into()).unwrap().to_string());
    command.set_parameters(
        Parameters::try_from_lvalue(&map.get(&PARAMETERS.into()).unwrap(), &lattice).await?,
    );
    let params = command.get_parameters().get_params_as_lvalue();
    let params_list = command.get_parameters().get_labels();
    let lv_exec: LValue = parse(
        &format!(
            "(lambda {} (exec-command '{} {}))",
            params,
            command.get_label(),
            {
                let mut str = String::new();
                for p in &params_list {
                    str.push_str(p.as_str());
                    str.push(' ');
                }
                str
            }
        ),
        &mut env,
    )
    .await?;

    let exec = eval(&expand(&lv_exec, true, &mut env).await?, &mut env, None).await?;

    command.set_body(exec);

    let lv_model: LValue = match map.get(&MODEL.into()) {
        None => parse(&format!("(lambda {} nil)", params), &mut env).await?,
        Some(model) => parse(&format!("(lambda {} {})", params, model), &mut env).await?,
    };
    let model = eval(&expand(&lv_model, true, &mut env).await?, &mut env, None).await?;
    command.set_model(model);

    let lv_cost: LValue = match map.get(&COST.into()) {
        None => parse(&format!("(lambda {} 1)", params), &mut env).await?,
        Some(model) => {
            let model = first(&env, &[model.clone()])?;
            parse(&format!("(lambda {} {})", params, model), &mut env).await?
        }
    };
    let cost = eval(&expand(&lv_cost, true, &mut env).await?, &mut env, None).await?;
    command.set_cost(cost);

    ctx.domain
        .write()
        .await
        .add_command(command.get_label().to_string(), command)?;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_command_model(
    env: &LEnv,
    model: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let label: String = model.get(&NAME.into()).unwrap().try_into()?;
    let model = create_model(&env, model).await?;
    ctx.domain.write().await.add_command_model(label, model)?;
    Ok(())
}
#[async_scheme_fn]
pub async fn add_task(env: &LEnv, map: im::HashMap<LValue, LValue>) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            ADD_TASK,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;

    let mut env = ctx.get_empty_env();

    let mut task = Task::default();
    task.set_label(map.get(&NAME.into()).unwrap().to_string());
    task.set_parameters(
        Parameters::try_from_lvalue(
            map.get(&PARAMETERS.into()).unwrap_or(&LValue::Nil),
            &lattice,
        )
        .await?,
    );
    let params = task.get_parameters().get_params_as_lvalue();
    let params_list = task.get_parameters().get_labels();
    let lv_exec: LValue = parse(
        &format!(
            "(lambda {} (exec-task '{} {}))",
            params,
            task.get_label(),
            {
                let mut str = String::new();
                for p in params_list {
                    str.push_str(p.as_str());
                    str.push(' ');
                }
                str
            }
        ),
        &mut env,
    )
    .await?;
    let exec = eval(&expand(&lv_exec, true, &mut env).await?, &mut env, None).await?;

    task.set_body(exec);

    if let Some(model) = map.get(&MODEL.into()) {
        let lv = list![LPrimitive::DefLambda.into(), params, model.clone()];
        let model = eval(&expand(&lv, true, &mut env).await?, &mut env, None).await?;
        task.set_model(model);
    }

    ctx.domain
        .write()
        .await
        .add_task(task.get_label().to_string(), task)?;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_task_model(
    env: &LEnv,
    model: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let label: String = model.get(&NAME.into()).unwrap().try_into()?;
    let model = create_model(&env, model).await?;
    ctx.domain.write().await.add_task_model(label, model)?;
    Ok(())
}

/// Defines a method in RAE environment.
#[async_scheme_fn]
pub async fn add_method(env: &LEnv, map: im::HashMap<LValue, LValue>) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            ADD_METHOD,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN)?;
    let lattice: RefTypeLattice = ctx.state.get_ref_lattice().await;
    let mut new_env = ctx.get_empty_env();
    let parameters = map.get(&PARAMETERS.into()).unwrap_or(&LValue::Nil).clone();
    let task_label = car(
        &new_env,
        &[map
            .get(&TASK.into())
            .ok_or_else(|| {
                LRuntimeError::new(
                    ADD_METHOD,
                    ":task is missing in the definition of the method.",
                )
            })?
            .clone()],
    )?
    .to_string();
    let body = car(
        &new_env,
        &[map.get(&BODY.into()).unwrap_or(&LValue::Nil).clone()],
    )?;
    let label = map
        .get(&NAME.into())
        .ok_or_else(|| {
            LRuntimeError::new(
                ADD_METHOD,
                ":name is missing in the definition of the method.",
            )
        })?
        .to_string();
    //Definition of the method
    let mut method = Method {
        label,
        task_label,
        parameters: Parameters::try_from_lvalue(&parameters, &lattice).await?,
        ..Default::default()
    };
    let conds = match map.get(&PRE_CONDITIONS.into()) {
        None => {
            let test = generate_test_type_expr(env, &[parameters.clone()]).await?;
            let expr = format!(
                "(lambda {} (do {}))",
                method.parameters.get_params_as_lvalue(),
                test
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
        Some(conds) => {
            let test = generate_test_type_expr(env, &[parameters.clone()]).await?;
            let mut str_conds = "(do".to_string();
            if let LValue::List(conds) = conds {
                for cond in conds.iter() {
                    str_conds.push_str(format!("(check {})", cond).as_str());
                }
                str_conds.push(')');
            } else {
                return Err(LRuntimeError::default());
            }
            let expr = format!(
                "(lambda {} (do {} {}))",
                method.parameters.get_params_as_lvalue(),
                test,
                str_conds
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
    };
    method.lambda_pre_conditions = conds;

    let score = match map.get(&SCORE.into()) {
        None => {
            let expr = format!("(lambda {} 0)", method.parameters.get_params_as_lvalue(),);
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
        Some(score) => {
            let expr = format!(
                "(lambda {} {})",
                method.parameters.get_params_as_lvalue(),
                car(env, &[score.clone()])?
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
    };
    method.lambda_score = score;

    /*let conds = cons(
        &LEnv::default(),
        &[
            method.lambda_pre_conditions.clone(),
            method.parameters.get_params_as_lvalue(),
        ],
    )?;*/

    let expr = format!(
        "(lambda {} (do {} {}))",
        method.parameters.get_params_as_lvalue(),
        LLambda::try_from(&method.lambda_pre_conditions)
            .unwrap()
            .get_body(),
        body
    );

    method.lambda_body = eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?;

    ctx.domain
        .write()
        .await
        .add_method(method.label.clone(), method)?;

    Ok(())
}

pub enum ModelType {
    PDDL,
    OM,
}

impl TryFrom<&str> for ModelType {
    type Error = LRuntimeError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "pddl" => Ok(Self::PDDL),
            "om" => Ok(Self::OM),
            _ => Err(LRuntimeError::default()),
        }
    }
}

/// Defines a lambda in RAE environment.
#[async_scheme_fn]
pub async fn add_lambda(env: &LEnv, label: String, lambda: &LValue) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN).unwrap();
    let mut env = ctx.get_empty_env();
    let expanded = expand(lambda, true, &mut env).await?;
    let mut e = get_root_env().await;
    let result = eval(&expanded, &mut e, None).await?;
    if let LValue::Lambda(_) = &result {
        ctx.domain.write().await.add_lambda(label, result);
    }
    Ok(())
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
#[async_scheme_fn]
pub async fn add_facts(env: &LEnv, map: im::HashMap<LValue, LValue>) -> Result<(), LRuntimeError> {
    let state = &env.get_context::<ModDomain>(MOD_DOMAIN)?.state;

    let mut inner_dynamic = PartialState {
        inner: Default::default(),
        _type: Some(StateType::InnerDynamic),
    };

    for (k, v) in &map {
        /*let mut is_instance: bool = false;
        if let LValue::List(key) = k {
            if key[0] == LValue::from(INSTANCE) {
                let instances: Vec<LValue> = v.try_into()?;
                for e in instances {
                    state
                        .add_instance(&key[1].to_string(), &e.to_string())
                        .await
                }
                is_instance = true;
            }
        }
        if !is_instance {
            inner_dynamic.insert(k.try_into()?, v.try_into()?);
        }*/
        inner_dynamic.insert(k.try_into()?, v.try_into()?);
    }

    state.update_state(inner_dynamic).await;

    Ok(())
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
#[async_scheme_fn]
pub async fn add_static_facts(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let state = &env.get_context::<ModDomain>(MOD_DOMAIN)?.state;

    let mut inner_static = PartialState {
        inner: Default::default(),
        _type: Some(StateType::InnerStatic),
    };

    for (k, v) in &map {
        inner_static.insert(k.try_into()?, v.try_into()?);
    }

    state.update_state(inner_static).await;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_type(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN).unwrap();

    let (t, parent) = match args.len() {
        1 => (args[0].to_string(), None),
        2 => (args[0].to_string(), Some(args[1].to_string())),
        _ => return Err(LRuntimeError::wrong_number_of_args(ADD_TYPE, args, 1..2)),
    };

    ctx.state.add_type(&t, parent.as_deref()).await;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_types(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    for arg in args {
        match arg {
            LValue::List(list) => {
                if list.len() < 2 {
                    return Err(lruntimeerror!(
                        ADD_TYPES,
                        format!("an objects is defined by a symbol and a type, got {}", arg)
                    ));
                }
                let last = list.last().unwrap();
                for t in &list[0..list.len() - 1] {
                    //println!("new type: {}", t);
                    add_type(env, &[t.clone(), last.clone()]).await?;
                }
            }
            lv => {
                add_type(env, &[lv.clone()]).await?;
            }
        }
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn add_object(env: &LEnv, object: String, t: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN).unwrap();

    ctx.state.add_instance(&object, &t).await;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_objects(env: &LEnv, args: Vec<Vec<LValue>>) -> Result<(), LRuntimeError> {
    for list in args {
        if list.len() < 2 {
            return Err(lruntimeerror!(
                ADD_OBJECTS,
                format!(
                    "an objects is defined by a symbol and a type, got {}",
                    LValue::from(list)
                )
            ));
        }
        let last = list.last().unwrap();
        for obj in &list[0..list.len() - 1] {
            add_object(env, &[obj.clone(), last.clone()]).await?;
        }
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn add_resource(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModDomain>(MOD_DOMAIN).unwrap();

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ADD_RESOURCE, args, 1..2))?
        .try_into()?;

    let capacity: Capacity = match args.get(1) {
        None => 1,
        Some(lv) => lv.try_into()?,
    };

    ctx.resource_manager
        .new_resource(label, Some(capacity))
        .await;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_resources(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    for lv in args {
        match lv {
            LValue::Symbol(s) => {
                add_resource(env, &[s.clone().into()]).await?;
            }
            LValue::List(list) => {
                add_resource(env, list.as_slice()).await?;
            }
            _ => {
                return Err(LRuntimeError::not_in_list_of_expected_types(
                    ADD_RESOURCES,
                    lv,
                    vec![Symbol, List],
                ));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use sompas_core::test_utils::{test_expression_with_env, TestExpression};
    use sompas_core::{eval_init, get_root_env};
    use sompas_modules::advanced_math::ModAdvancedMath;
    use sompas_modules::io::ModIO;
    use sompas_modules::utils::ModUtils;
    use sompas_structs::lenv::ImportType::WithoutPrefix;
    use sompas_structs::lenv::LEnv;

    async fn init_env_and_ctxs() -> LEnv {
        let mut env = get_root_env().await;

        env.import_module(ModUtils::default(), WithoutPrefix);

        env.import_module(ModAdvancedMath::default(), WithoutPrefix);

        let mut ctx = ModDomain::default();
        ctx.empty_env = ModDomain::init_empty_env().await;

        env.import_module(ctx, WithoutPrefix);

        env.import_module(ModIO::default(), WithoutPrefix);
        eval_init(&mut env).await;
        env
    }

    #[tokio::test]
    async fn test_macro_def_task() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_TASK,
            dependencies: vec![],
            expression: "(def-task t_navigate_to (:params (?r robot) (?x int) (?y int)))",
            expected: "(add-task \
                            (map '(\
                                (:name t_navigate_to)\
                                (:params ((?r robot) (?x int) (?y int)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_def_state_function() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_STATE_FUNCTION,
            dependencies: vec![],
            expression:
                "(def-state-function sf (:params (?a object) (?b object)) (:result object))",
            expected: "(add-state-function \
                            (map '(\
                                (:name sf)\
                                (:params ((?a object) (?b object)))\
                                (:result (object)))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => {}
            Err(e) => {
                println!("err : {}", e);
                return Err(e);
            }
        };

        let macro_to_test = TestExpression {
            inner: MACRO_DEF_STATE_FUNCTION,
            dependencies: vec![],
            expression: "(def-state-function sf (:result object))",
            expected: "(add-state-function \
                        (map '(\
                            (:name sf)\
                            (:result (object)))))",
            result: "nil",
        };
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_command() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_COMMAND,
            dependencies: vec![],
            expression: "(def-command pick_package (:params (?r robot) (?p package)))",
            expected: "(add-command \
                            (map '(\
                                (:name pick_package)\
                                (:params ((?r robot) (?p package)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_command_pddl_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_COMMAND_PDDL_MODEL,
            dependencies: vec![MACRO_DEF_COMMAND],
            expression: "(def-command-pddl-model pick
                          (:params (?obj ball) (?room room) (?gripper gripper))
                          (:pre-conditions
                            (= (at ?obj) ?room)
                            (= (at-robby) ?room)
                            (= (carry ?gripper) no_ball))
                          (:effects
                            (begin
                                (assert `(carry ,?gripper) ?obj)
                                (assert `(at ,?obj) no_place))))",
            expected: "(add-command-model\
                (map '(\
                    (:name pick)\
                    (:model-type pddl)\
                    (:params ((?obj ball) (?room room) (?gripper gripper)))\
                    (:pre-conditions ((= (at ?obj) ?room) (= (at-robby) ?room) (= (carry ?gripper) no_ball)))\
                    (:effects ((begin (assert `(carry ,?gripper) ?obj) (assert `(at ,?obj) no_place)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;

        eval(
            &parse(
                "(def-command pick (:params (?obj ball) (?room room) (?gripper gripper)))",
                &mut env,
            )
            .await?,
            &mut env,
            None,
        )
        .await?;

        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_command_om_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_COMMAND_OM_MODEL,
            dependencies: vec![MACRO_DEF_COMMAND],
            expression: "(def-command-om-model pick
                            (:params (?r robot))
                            (:body
                                (do
                                    (check (> (robot.battery ?r) 0.4))
                                    (assert (robot.busy ?r) true))))",
            expected: "(add-command-model
                         (map '(
                            (:name pick) 
                            (:model-type om) 
                            (:params ((?r robot))) 
                            (:body ((do 
                              (check (> (robot.battery ?r) 0.4)) 
                              (assert (robot.busy ?r) true)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;
        eval(
            &parse(
                "(def-command pick (:params (?obj ball) (?room room) (?gripper gripper)))",
                &mut env,
            )
            .await?,
            &mut env,
            None,
        )
        .await?;
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_task_pddl_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_TASK_PDDL_MODEL,
            dependencies: vec![MACRO_DEF_TASK],
            expression: "(def-task-pddl-model pick
                          (:params (?obj ball) (?room room) (?gripper gripper))
                          (:pre-conditions
                            (= (at ?obj) ?room)
                            (= (at-robby) ?room)
                            (= (carry ?gripper) no_ball))
                          (:effects
                            (begin
                                (assert 'carry ?gripper ?obj)
                                (assert 'at ?obj no_place))))",
            expected: "(add-task-model\
                (map '(\
                    (:name pick)\
                    (:model-type pddl)\
                    (:params ((?obj ball) (?room room) (?gripper gripper)))\
                    (:pre-conditions ((= (at ?obj) ?room) (= (at-robby) ?room) (= (carry ?gripper) no_ball)))\
                    (:effects ((begin (assert 'carry ?gripper ?obj) (assert `at ?obj no_place)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;

        eval(
            &parse(
                "(def-task pick (:params (?obj ball) (?room room) (?gripper gripper)))",
                &mut env,
            )
            .await?,
            &mut env,
            None,
        )
        .await?;

        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_command_om_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_TASK_OM_MODEL,
            dependencies: vec![MACRO_DEF_TASK],
            expression: "(def-task-om-model pick
                            (:params (?r robot))
                            (:body
                                (do
                                    (check (> (robot.battery ?r) 0.4))
                                    (assert (robot.busy ?r) true))))",
            expected: "(add-task-model
                         (map '(
                            (:name pick) 
                            (:model-type om) 
                            (:params ((?r robot))) 
                            (:body ((do 
                              (check (> (robot.battery ?r) 0.4)) 
                              (assert (robot.busy ?r) true)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;
        eval(
            &parse(
                "(def-task pick (:params (?obj ball) (?room room) (?gripper gripper)))",
                &mut env,
            )
            .await?,
            &mut env,
            None,
        )
        .await?;
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }

    #[tokio::test]
    async fn test_macro_def_method() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_DEF_METHOD,
            dependencies: vec![MACRO_DEF_TASK],
            expression: "(def-method m_navigate_to (:task t_navigate_to)
            (:params (?r robot) (?x float) (?y float))
            (:pre-conditions (robot.available ?r) (< ?x 10) (< ?y 10))
            (:score 0)
            (:body
            (begin
                (navigate_to ?r ?x ?y))))",
            expected: "(add-method
 (map '(
    (:name m_navigate_to)
    (:task (t_navigate_to))
    (:params ((?r robot) (?x float) (?y float)))
    (:pre-conditions ((robot.available ?r) (< ?x 10) (< ?y 10)))
    (:score (0))
    (:body
    ((begin
        (navigate_to ?r ?x ?y)))))))",
            result: "nil",
        };

        let mut env = init_env_and_ctxs().await;

        eval(
            &parse(
                "(def-task t_navigate_to (:params (?r robot) (?x float) (?y float)))",
                &mut env,
            )
            .await?,
            &mut env,
            None,
        )
        .await?;
        match test_expression_with_env(macro_to_test, &mut env, true).await {
            Ok(_) => Ok(()),
            Err(e) => {
                println!("err : {}", e);
                Err(e)
            }
        }
    }
}
