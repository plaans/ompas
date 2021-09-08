use ompas_lisp::core::{parse, LEnv};
use ompas_lisp::structs::LError;

const EXAMPLE: &str = "(begin (+ 3 3) (+ 6 2) (3))";
const EXAMPLE_2: &str = "(if (> 1 3) nil (+ 3 6))";
const EXAMPLE_3: &str = "(let ((x 3) (y 4)) (+ x y))";
const EXAMPLE_4: &str = "(let* ((x 3) (y (+ x 1))) (+ x y))";

fn list_of_example() -> Vec<&'static str> {
    vec![EXAMPLE, EXAMPLE_2, EXAMPLE_3, EXAMPLE_4]
}

#[test]
fn test_pretty_print_lvalue() -> Result<(), LError> {
    let (mut env, mut ctxs, lisp_init) = LEnv::root();

    for element in list_of_example() {
        let lvalue = parse(element, &mut env, &mut ctxs)?;
        println!("string: {}", element);
        println!("lvalue: {}", lvalue);
        println!("pretty: {}", lvalue.pretty_print("pretty: ".len()))
    }

    Ok(())
}
