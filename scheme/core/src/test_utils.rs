use crate::{eval, get_root_env, parse};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;

pub struct TestExpression {
    pub inner: &'static str,
    pub dependencies: Vec<&'static str>,
    pub expression: &'static str,
    pub expected: &'static str,
    pub result: &'static str,
}

pub async fn test_expression(test_expression: TestExpression) -> lruntimeerror::Result<()> {
    //root env
    let mut env = get_root_env().await;

    //Load dependencies
    for e in test_expression.dependencies {
        eval(&parse(e, &mut env).await?, &mut env, None).await?;
    }

    //Load macro
    eval(
        &parse(test_expression.inner, &mut env).await?,
        &mut env,
        None,
    )
    .await?;

    //Expand expression
    let expanded = parse(test_expression.expression, &mut env).await?;

    let expected = parse(test_expression.expected, &mut env).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-extended: {}\n\
        \t-expected: {}",
        test_expression.expression, expanded, expected
    );

    assert_eq!(expanded, expected);

    let result = eval(&expanded, &mut env, None).await?;
    let expected_result = parse(test_expression.result, &mut env).await?;
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
    eval_result: bool,
) -> lruntimeerror::Result<()> {
    for e in test_expression.dependencies {
        eval(&parse(e, env).await?, env, None).await?;
    }

    //Load macro
    eval(&parse(test_expression.inner, env).await?, env, None).await?;

    //Expand expression
    let expanded = parse(test_expression.expression, env).await?;

    let expected = parse(test_expression.expected, env).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-expanded: {}\n\
        \t-expected: {}",
        test_expression.expression, expanded, expected
    );

    assert_eq!(expanded, expected,);

    let result = eval(&expanded, env, None).await?;
    let expected_result = match eval_result {
        true => eval(&parse(test_expression.result, env).await?, env, None).await?,
        false => parse(test_expression.result, env).await?,
    };
    println!(
        "\t-result: {}\n\
            \t-expected result: {}\n",
        result, expected_result
    );

    assert_eq!(result, expected_result);

    Ok(())
}
