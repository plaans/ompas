use crate::core::{eval, parse, ContextCollection, LEnv};
use crate::structs::LError;

pub struct TestExpression {
    pub inner: &'static str,
    pub dependencies: Vec<&'static str>,
    pub expression: &'static str,
    pub expanded: &'static str,
    pub result: &'static str,
}

pub async fn test_expression(test_expression: TestExpression) -> Result<(), LError> {
    //root env
    let (mut env, mut ctxs) = LEnv::root().await;

    //Load dependencies
    for e in test_expression.dependencies {
        eval(&parse(e, &mut env, &mut ctxs).await?, &mut env, &mut ctxs).await?;
    }

    //Load macro
    eval(
        &parse(test_expression.inner, &mut env, &mut ctxs).await?,
        &mut env,
        &mut ctxs,
    )
    .await?;

    //Expand expression
    let expanded = parse(test_expression.expression, &mut env, &mut ctxs).await?;

    let expected = parse(test_expression.expanded, &mut env, &mut ctxs).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-extended: {}\n\
        \t-expected: {}",
        test_expression.expression, expanded, expected
    );

    assert_eq!(expanded, expected,);

    let result = eval(&expanded, &mut env, &mut ctxs).await?;
    let expected_result = parse(test_expression.result, &mut env, &mut ctxs).await?;
    println!(
        "\t-result: {}\n\
            \t-expected result: {}\n",
        result, expected_result
    );

    assert_eq!(result, expected_result);

    Ok(())
}

pub async fn test_expression_with_env(
    test_expression: TestExpression,
    env: &mut LEnv,
    ctxs: &mut ContextCollection,
    eval_result: bool,
) -> Result<(), LError> {
    for e in test_expression.dependencies {
        eval(&parse(e, env, ctxs).await?, env, ctxs).await?;
    }

    //Load macro
    eval(&parse(test_expression.inner, env, ctxs).await?, env, ctxs).await?;

    //Expand expression
    let expanded = parse(test_expression.expression, env, ctxs).await?;

    let expected = parse(test_expression.expanded, env, ctxs).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-extended: {}\n\
        \t-expected: {}",
        test_expression.expression, expanded, expected
    );

    assert_eq!(expanded, expected,);

    let result = eval(&expanded, env, ctxs).await?;
    let expected_result = match eval_result {
        true => eval(&parse(test_expression.result, env, ctxs).await?, env, ctxs).await?,
        false => parse(test_expression.result, env, ctxs).await?,
    };
    println!(
        "\t-result: {}\n\
            \t-expected result: {}\n",
        result, expected_result
    );

    assert_eq!(result, expected_result);

    Ok(())
}
