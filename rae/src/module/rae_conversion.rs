use crate::context::rae_env::RAEEnv;
use crate::module::{CtxRae, MOD_RAE};
use crate::planning::conversion::convert_domain_to_chronicle_hierarchy;
use crate::planning::conversion::pre_processing::{pre_processing, transform_lambda_expression};
use crate::planning::conversion::processing::{
    convert_if, convert_lvalue_to_expression_chronicle, MetaData,
};
use crate::planning::structs::traits::FormatWithSymTable;
use crate::planning::structs::{ChronicleHierarchy, ConversionContext};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::expand;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::WrongNumberOfArgument;
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::dyn_async;
use std::time::SystemTime;

pub const RAE_CONVERT_EXPR: &str = "convert-expr";
pub const RAE_CONVERT_DOMAIN: &str = "convert-domain";
pub const RAE_PRE_PROCESS_LAMBDA: &str = "pre-process-lambda";
pub const RAE_PRE_PROCESS_EXPR: &str = "pre-process-expr";
pub const RAE_PRE_PROCESS_DOMAIN: &str = "pre-process-domain";
pub const RAE_CONVERT_COND_EXPR: &str = "convert-cond-expr";

#[macro_rules_attribute(dyn_async!)]
pub async fn convert_expr<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_CONVERT_EXPR,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut context: ConversionContext = ctx.get_conversion_context().await;

    let lv = expand(&args[0], true, &mut context.env).await?;

    let mut ch = ChronicleHierarchy::default();

    let time = SystemTime::now();
    let chronicle =
        convert_lvalue_to_expression_chronicle(&lv, &context, &mut ch, MetaData::new(true, false))?;
    let time = time.elapsed().expect("could not get time").as_micros();
    let string = chronicle.format_with_sym_table(&ch.sym_table);

    Ok(format!("{}\n\n Time to convert: {} µs.", string, time).into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn convert_domain<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let time = SystemTime::now();
    let ch = convert_domain_to_chronicle_hierarchy(context)?;
    let time = time.elapsed().expect("could not get time").as_micros();

    Ok(format!("{}\n\nTime to convert: {} µs.", ch, time).into())
    //Ok(format!("Time to convert: {} µs.", time).into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn convert_cond_expr<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_CONVERT_COND_EXPR,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    let mut ch = ChronicleHierarchy::default();

    let result = convert_if(&args[0], &context, &mut ch)?;

    Ok(result.format_with_sym_table(&ch.sym_table).into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn pre_process_lambda<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_PRE_PROCESS_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    transform_lambda_expression(&args[0], context.env)
}

#[macro_rules_attribute(dyn_async!)]
pub async fn pre_process_expr<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_PRE_PROCESS_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    pre_processing(&args[0], &context, &mut ChronicleHierarchy::default())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn pre_process_domain<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    //let mut context: Context = ctx.into();
    let mut str = "pre-processing of the domain:\n".to_string();
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let rae_env: &RAEEnv = &ctx.own_rae_env().await;
    let context: ConversionContext = ctx.get_conversion_context().await;

    for (action_label, action) in rae_env.domain_env.get_actions() {
        let pre_processed = pre_processing(
            action.get_sim(),
            &context,
            &mut ChronicleHierarchy::default(),
        )?;

        str.push_str(
            format!(
                "{}:\n\tbefore: {}\n\tafter: {}\n",
                action_label,
                action.get_sim().format("\tbefore: ".len()),
                pre_processed.format("\tafter: ".len()),
            )
            .as_str(),
        );
    }

    Ok(str.into())
}
