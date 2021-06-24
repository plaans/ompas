pub const MACRO_DEF_STATE_FUNCTION: &str = "(defmacro def-state-function (lambda args
    (let ((label (car args))
           (params (cdr args)))
        (quasiquote (rae-add-state-function (unquote label)
         (lambda (unquote params)
          (unquote (cons rae-get-state-variable (cons (quasiquote (quote (unquote label))) params)))))))))";

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
                                                                    (lambda (unquote params) (unquote (cons rae-exec-command\
                                                                            (cons label params)))))))))";
pub const DEF_ACTION: &str = "defmethod";
