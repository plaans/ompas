(begin
    (define y
        (begin
            (define x 1)
            x))
    (define x 2)
    (+ y x))