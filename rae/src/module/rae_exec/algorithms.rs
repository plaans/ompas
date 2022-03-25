use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;

pub const LAMBDA_PROGRESS: &str = "
(define progress (lambda task
    (let* ((result (select task))
            (first_m (first result))
            (task_id (second result)))

            (if (null? first_m)
                (err err::no-applicable-method)
                (begin
                    (print \"trying \" first_m)
                    (define result (enr first_m))
                    (print \"tried fist method of \" task_id)
                    (if (err? result)
                        (retry task_id)
                        (rae-set-success-for-task task_id)))))))";

pub const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";

pub const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin 
            (print \"Retrying task \" task_id)
            (if (null? new_method) ; if there is no method applicable
            nil
            (if (enr new_method)
                (rae-set-success-for-task task_id)
                (rae-retry task_id)))))))";

//Access part of the environment

pub const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get rae-task-methods-map label)))";

pub async fn progress(args: &[LValue], env: &LEnv) -> LResult {}

pub async fn select(args: &[LValue], env: &LEnv) -> LResult {}

pub async fn retry(args: &[LValue], env: &LEnv) -> LResult {}
