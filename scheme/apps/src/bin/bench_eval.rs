use sompas_core::{eval, get_root_env, parse};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::time::SystemTime;

#[tokio::main]
pub async fn main() {
    //let env = get_root_env().await;
    let _ = test_fact(100).await;
    let _ = test_lisp_integration().await;
}

fn create_list_test() -> Vec<(&'static str, LValue)> {
    let is_tests = vec![
        (
            "(quote (testing 1 (2.0) -3.14e159))",
            LValue::from(vec![
                "testing".into(),
                1.into(),
                vec![2.0].into(),
                LValue::from(-3.14e159),
            ]),
        ),
        ("(+ 2 2)", 4.into()),
        ("(+ (* 2 100) (* 1 10))", 210.into()),
        ("(if (> 6 5) (+ 1 1) (+ 2 2))", 2.into()),
        ("(if (< 6 5) (+ 1 1) (+ 2 2))", 4.into()),
        ("(define x 3)", LValue::Nil),
        ("x", 3.into()),
        ("(+ x x)", 6.into()),
        ("(begin (define x 1) (define x (+ x 1)) (+ x 1))", 3.into()),
        ("((lambda (x) (+ x x)) 5)", 10.into()),
        ("(define twice (lambda (x) (* 2 x)))", LValue::Nil),
        ("(twice 5)", 10.into()),
        ("(define compose (lambda (f g) (lambda (x) (f (g x)))))", LValue::Nil),
        ("((compose list twice) 5)", vec![10].into()),
        ("(define repeat (lambda (f) (compose f f)))", LValue::Nil),
        ("((repeat twice) 5)", 20.into()), ("((repeat (repeat twice)) 5)", 80.into()),
        (
            "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))",
            LValue::Nil,
        ),
        ("(fact 3)", 6.into()),
        //("(fact 50)", 30414093201713378043612608166064768844377641568960512000000000000.into()),
        (
            "(define abs (lambda (n) ((if (> n 0) + -) 0 n)))",
            LValue::Nil,
        ),
        (
            "(list (abs -3) (abs 0) (abs 3))",
            vec![3, 0, 3].into(),
        ),
        ("(define combine (lambda (f) \
        (lambda (x y) \
          (if (null? x) (quote ()) \
              (f (list (car x) (car y)) \
                 ((combine f) (cdr x) (cdr y)))))))", LValue::Nil),
        ("(define zip (combine cons))", LValue::Nil),
        ("(zip (list 1 2 3 4) (list 5 6 7 8))", vec![vec![1, 5], vec![2, 6], vec![3, 7], vec![4, 8]].into()),
        ("(define riff-shuffle (lambda (deck) (begin \
        (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) \
        (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq))))) \
        (define mid (lambda (seq) (/ (len seq) 2))) \
        ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))", LValue::Nil),
        /*("(riff-shuffle (list 1 2 3 4 5 6 7 8))", vec![1, 5, 2, 6, 3, 7, 4, 8].into()),
        ("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))",  vec![1, 3, 5, 7, 2, 4, 6, 8].into()),
        ("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", vec![1,2,3,4,5,6,7,8].into()),*/
    ];
    is_tests
}

async fn test_fact(n: i64) -> lruntimeerror::Result<()> {
    let mut env = get_root_env().await;
    let lv = parse(
        "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))",
        &mut env,
    )
    .await?;
    let _ = eval(&lv, &mut env, None).await?;
    let time = SystemTime::now();
    let lv = parse(format!("(fact {n})").as_str(), &mut env).await?;
    let parse = time.elapsed().unwrap().as_secs_f64();
    let _ = eval(&lv, &mut env, None).await?;
    let end = time.elapsed().unwrap().as_secs_f64();
    println!(
        "eval({lv}) took {:.3} µs ({:.1} % to parse)",
        end * 1_000_000.0,
        parse / end * 100.0
    );

    Ok(())
}

async fn test_lisp_integration() -> lruntimeerror::Result<()> {
    let mut env = get_root_env().await;
    let list = create_list_test();
    let mut elements = Vec::with_capacity(create_list_test().capacity());
    for (str, lv) in list {
        elements.push((parse(str, &mut env).await?, lv))
    }
    for _ in 1..1000 {
        for (e, r) in &elements {
            //stdout.write_all(b"parsing done\n");
            let result = match eval(e, &mut env, None).await {
                Ok(s) => s,
                Err(e) => {
                    return Err(LRuntimeError::new(
                        "test_lisp_integration",
                        format!("{}:{}:{:?}", e, r, e),
                    ))
                }
            };
            assert_eq!(result, r.clone());
        }
    }
    Ok(())
}
