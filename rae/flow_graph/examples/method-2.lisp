(begin
    (define x (read-state 'at-robby))
    (define y (read-state 'at ?ball))
    (define r (= x y))
    (assert 'at ?ball y))