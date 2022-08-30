use crate::rae_user::{CtxRaeUser, MOD_RAE_USER};
use aries_planning::chronicles::ChronicleKind;
use ompas_rae_planning::conversion::convert_domain_to_chronicle_hierarchy;
use ompas_rae_planning::conversion::post_processing::post_processing;
use ompas_rae_planning::conversion::pre_processing::{pre_processing, transform_lambda_expression};
use ompas_rae_planning::conversion::processing::{
    convert_if, convert_lvalue_to_expression_chronicle, MetaData,
};
use ompas_rae_planning::structs::chronicle::ChronicleTemplate;
use ompas_rae_planning::structs::traits::FormatWithSymTable;
use ompas_rae_planning::structs::{ConversionCollection, ConversionContext};
use sompas_core::expand;
use sompas_macros::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::time::SystemTime;

#[async_scheme_fn]
pub async fn convert_expr(env: &LEnv, expr: &LValue) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    let mut context: ConversionContext = ctx.get_conversion_context().await;

    let lv = expand(expr, true, &mut context.env).await?;

    let mut ch = ConversionCollection::default();

    let time = SystemTime::now();
    let mut chronicle = ChronicleTemplate::new(&mut ch, "unnamed_chronicle", ChronicleKind::Method);

    let pre_processed = pre_processing(&lv, &context, &mut ch)?;
    let ec = convert_lvalue_to_expression_chronicle(
        &pre_processed,
        &context,
        &mut ch,
        MetaData::new(true, false),
    )?;

    chronicle.absorb_expression_chronicle(ec);

    post_processing(&mut chronicle, &context, &mut ch)?;
    let time = time.elapsed().expect("could not get time").as_micros();
    let string = chronicle.format(&ch.sym_table, true);

    Ok(format!("{}\n\n Time to convert: {} µs.", string, time))
}

#[async_scheme_fn]
pub async fn convert_domain(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let context: ConversionContext = ctx.get_conversion_context().await;
    let time = SystemTime::now();
    let ch = convert_domain_to_chronicle_hierarchy(context)?;
    let time = time.elapsed().expect("could not get time").as_micros();
    Ok(format!("{}\n\nTime to convert: {} µs.", ch, time))
}

#[async_scheme_fn]
pub async fn convert_cond_expr(env: &LEnv, expr: &LValue) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    let mut ch = ConversionCollection::default();

    let result = convert_if(expr, &context, &mut ch)?;

    Ok(result.format(&ch.sym_table, true))
}

#[async_scheme_fn]
pub async fn pre_process_lambda(env: &LEnv, expr: &LValue) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    transform_lambda_expression(expr, context.env)
}

#[async_scheme_fn]
pub async fn pre_process_expr(env: &LEnv, expr: &LValue) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    pre_processing(expr, &context, &mut ConversionCollection::default())
}

#[async_scheme_fn]
pub async fn pre_process_domain(env: &LEnv) -> Result<String, LRuntimeError> {
    //let mut context: Context = ctx.into();
    let mut str = "pre-processing of the domain:\n".to_string();
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let context: ConversionContext = ctx.get_conversion_context().await;

    for (action_label, action) in ctx.rae_domain.read().await.get_actions() {
        let pre_processed = pre_processing(
            action.get_model(),
            &context,
            &mut ConversionCollection::default(),
        )?;

        str.push_str(
            format!(
                "{}:\n\tbefore: {}\n\tafter: {}\n",
                action_label,
                action.get_model().format("\tbefore: ".len()),
                pre_processed.format("\tafter: ".len()),
            )
            .as_str(),
        );
    }

    Ok(str)
}
