use crate::{eval, get_root_env, parse};
use ompas_middleware::logger::LogClient;
use ompas_middleware::Master;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use std::fmt::{Display, Formatter};

enum KindExpr {
    Macro,
    Lambda,
}

pub struct Expr {
    label: &'static str,
    expr: &'static str,
    kind: KindExpr,
}

impl Expr {
    pub fn _macro(label: &'static str, expr: &'static str) -> Self {
        Self {
            label,
            expr,
            kind: KindExpr::Macro,
        }
    }

    pub fn _lambda(label: &'static str, expr: &'static str) -> Self {
        Self {
            label,
            expr,
            kind: KindExpr::Lambda,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            match self.kind {
                KindExpr::Macro => "defmacro",
                KindExpr::Lambda => "define",
            },
            self.label,
            self.expr
        )
    }
}

pub struct TestExpression<'a> {
    pub inner: Expr,
    pub dependencies: Vec<Expr>,
    pub expression: &'a str,
    pub expected: &'a str,
    pub result: &'a str,
}

pub async fn test_expression(test_expression: TestExpression<'_>) -> lruntimeerror::Result<()> {
    //root env
    Master::reinit();
    let log: LogClient = LogClient::new("TEST", "TEST").await;
    let mut env: LEnv = get_root_env().await;
    env.log = log;

    //Load dependencies
    for e in test_expression.dependencies {
        eval(&parse(&e.to_string(), &mut env).await?, &mut env, None).await?;
    }

    //Load macro
    eval(
        &parse(&test_expression.inner.to_string(), &mut env).await?,
        &mut env,
        None,
    )
    .await?;

    //Expand expression
    let expanded = parse(test_expression.expression, &mut env.clone()).await?;

    let expected = parse(test_expression.expected, &mut env.clone()).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-expanded: {}\n\
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
    test_expression: TestExpression<'_>,
    env: &mut LEnv,
    eval_result: bool,
) -> lruntimeerror::Result<()> {
    for e in test_expression.dependencies {
        eval(&parse(&e.to_string(), env).await?, env, None).await?;
    }

    //Load macro
    eval(
        &parse(&test_expression.inner.to_string(), env).await?,
        env,
        None,
    )
    .await?;

    //Expand expression
    let expanded = parse(test_expression.expression, &mut env.clone()).await?;

    let expected = parse(test_expression.expected, &mut env.clone()).await?;

    println!(
        "test_macro:\n\
        \t-expression: {}\n\
        \t-expanded: {}\n\
        \t-expected: {}",
        test_expression.expression, expanded, expected
    );

    assert_eq!(expanded, expected);

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
