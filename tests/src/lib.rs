#[cfg(test)]
mod tests {
    use ompas_lisp::core::{eval, load_module, parse, ContextCollection, RefLEnv};
    use ompas_lisp::structs::LCoreOperator::Define;
    use ompas_lisp::structs::LValue;
    use ompas_modules::doc::Documentation;
    use ompas_modules::io::CtxIo;
    use ompas_modules::math::CtxMath;
    use std::cell::Ref;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    fn create_env_and_ctxs() -> (RefLEnv, ContextCollection) {
        let mut root_env: RefLEnv = RefLEnv::root();
        let mut ctxs: ContextCollection = Default::default();
        let lisp_init = &mut Default::default();
        load_module(&mut root_env, &mut ctxs, CtxMath::default(), lisp_init);

        let env = RefLEnv::new_from_outer(root_env.clone());
        (env, ctxs)
    }

    fn create_list_test() -> Vec<(&'static str, LValue)> {
        let is_tests = vec![
            (
                "(quote (testing 1 (2.0) -3.14e159))",
                LValue::from(vec![
                    "testing".into(),
                    1.into(),
                    vec![2.0.into()].into(),
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
            //("(begin (define x 1) (set! x (+ x 1)) (+ x 1))", 3.into()), //TODO: fix
            ("((lambda (x) (+ x x)) 5)", 10.into()),
            ("(define twice (lambda (x) (* 2 x)))", LValue::Nil),
            ("(twice 5)", 10.into()),
            //("(define compose (lambda (f g) (lambda (x) (f (g x)))))", LValue::Nil),
            //("((compose list twice) 5)", vec![10.into()].into()),
            //("(define repeat (lambda (f) (compose f f)))", LValue::Nil),
            //("((repeat twice) 5)", 20.into()), ("((repeat (repeat twice)) 5)", 80.into()),
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
                vec![3.into(), 0.into(), 3.into()].into(),
            ),
            /*("""(define combine (lambda (f)
            (lambda (x y)
              (if (null? x) (quote ())
                  (f (list (car x) (car y))
                     ((combine f) (cdr x) (cdr y)))))))""", None),
                ("(define zip (combine cons))", None),
                ("(zip (list 1 2 3 4) (list 5 6 7 8))", [[1, 5], [2, 6], [3, 7], [4, 8]]),
                ("""(define riff-shuffle (lambda (deck) (begin
            (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
            (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
            (define mid (lambda (seq) (/ (length seq) 2)))
            ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))""", None),
                ("(riff-shuffle (list 1 2 3 4 5 6 7 8))", [1, 5, 2, 6, 3, 7, 4, 8]),
                ("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))",  [1, 3, 5, 7, 2, 4, 6, 8]),
                ("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", [1,2,3,4,5,6,7,8]),*/
        ];
        is_tests
    }

    #[test]
    fn test_lisp_integration() {
        let (mut env, mut ctxs) = create_env_and_ctxs();
        for element in create_list_test() {
            let lvalue = parse(element.0, &mut env, &mut ctxs).unwrap();
            //stdout.write_all(b"parsing done\n");
            let result = eval(&lvalue, &mut env, &mut ctxs).unwrap();
            assert_eq!(result, element.1)
        }
    }
}
