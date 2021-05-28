(define zip (lambda (l1 l2)
                (if (or (null? l1) (null? l2))
                    nil
                    (cons (list (car x) (car y))
                          (zip (cdr x) (cdr y)))))))

(define zip (lambda (l1 l2) (if (or (null? l1) (null? l2)) nil (cons (list (car x) (car y)) (zip (cdr x) (cdr y)))))))

(define unzip (lambda (l1 l2)
(defmacro zip (lambda (l1 l2) (quasiquote (if (of (null? (unquote l1) (null? (unquote l2))

(zip (list 4) (list 8))
=> (if (null? (4))