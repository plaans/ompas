use ompas_lisp::core::parse;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError;

const EXAMPLE: &str = "(begin (+ 3 3) (+ 6 2) (3))";
const EXAMPLE_2: &str = "(if (> 1 3) nil (+ 3 6))";
const EXAMPLE_3: &str = "(let ((x 3) (y 4)) (+ x y))";
const EXAMPLE_4: &str = "(let* ((x 3) (y (+ x 1))) (+ x y))";

fn list_of_example() -> Vec<&'static str> {
    vec![EXAMPLE, EXAMPLE_2, EXAMPLE_3, EXAMPLE_4]
}

#[tokio::main]
async fn main() -> Result<(), LError> {
    let mut env = LEnv::root().await;

    for element in list_of_example() {
        let lvalue = parse(element, &mut env).await?;
        println!("string: {}", element);
        println!("lvalue: {}", lvalue);
        println!("pretty: {}", lvalue.format("pretty: ".len()))
    }

    Ok(())
}
