(begin
    (define combine (lambda (f)
            (lambda (x y)
              (if (null? x) (quote ())
                  (f (list (car x) (car y))
                     ((combine f) (cdr x) (cdr y)))))))
    (define zip (combine cons)))
    (define riff-shuffle (lambda (deck) (begin
                (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
                (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
                (define mid (lambda (seq) (/ (len seq) 2)))
                ((combine append) (take (mid deck) deck) (drop (mid deck) deck))))))