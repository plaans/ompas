pub const MACRO_GENERATE_STATE_FUNCTION: &str = "(defmacro generate-state-function (lambda args
    (let ((label (car args))
           (params (cdr args)))
        (quasiquote (list (unquote label)
         (lambda (unquote params)
          (unquote (cons (quote rae-get-state-variable) (cons (quasiquote (quote (unquote label))) params)))))))))";

pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task \
                                        (lambda (l body) \
                                            (quasiquote (list (unquote l) (lambda (unquote (cdar body)) \
                                                (if (unquote (cadadr body)) \
                                                    (unquote (cadaddr body)) \
                                                    (quote (task is not applicable in the given state))))))))";

pub const MACRO_GENERATE_TASK_SIMPE: &str = "(defmacro generate-task-simple";

pub const MACRO_GENERATE_METHOD: &str = "(defmacro generate-method \
                                          (lambda (l body) \
                                            (let ((task-label (cadar body)) \
                                                  (params (cdadr body)) \
                                                  (body (cadaddr body))) \
                                                 (quasiquote (list (unquote l) \
                                                                    (quote (unquote task-label)) \
                                                                    (lambda (unquote params) \
                                                                            (unquote body)))))))";

pub const MACRO_GENERATE_ACTION: &str ="(defmacro generate-action \
                                        (lambda args \
                                            (let ((label (car args)) \
                                                  (params (cdr args))) \
                                                 (quasiquote (list (unquote label) \
                                                                    (lambda (unquote params) (unquote (cons (quote rae-exec-command)\
                                                                            (cons (quasiquote (quote (unquote label))) params)))))))))";

/*pub const MACRO_DEF_STATE_FUNCTION: &str = "(defmacro def-state-function (lambda args
    (let ((label (car args))
           (params (cdr args)))
        (quasiquote (rae-add-state-function (unquote label)
         (lambda (unquote params)
          (unquote (cons (quote rae-get-state-variable) (cons (quasiquote (quote (unquote label))) params)))))))))";

pub const MACRO_DEF_TASK: &str = "(defmacro def-task \
                                        (lambda (l body) \
                                            (quasiquote (rae-add-task (unquote l) (lambda (unquote (cdar body)) \
                                                (if (unquote (cadadr body)) \
                                                    (unquote (cadaddr body)) \
                                                    (quote (task is not applicable in the given state))))))))";
pub const DEF_TASK: &str = "deftask";

pub const MACRO_DEF_METHOD: &str = "(defmacro def-method \
                                          (lambda (l body) \
                                            (let ((task-label (cadar body)) \
                                                  (params (cdadr body)) \
                                                  (body (cadaddr body))) \
                                                 (quasiquote (rae-add-method (unquote l) \
                                                                    (unquote task-label) \
                                                                    (lambda (unquote params) \
                                                                            (unquote body)))))))";
pub const DEF_METHOD: &str = "defmethod";

pub const MACRO_DEF_ACTION: &str ="(defmacro def-action \
                                        (lambda args \
                                            (let ((label (car args)) \
                                                  (params (cdr args))) \
                                                 (quasiquote (rae-add-action (unquote label) \
                                                                    (lambda (unquote params) (unquote (cons (quote rae-exec-command)\
                                                                            (cons (quasiquote (quote (unquote label))) params)))))))))";
pub const DEF_ACTION: &str = "defmethod";*/
