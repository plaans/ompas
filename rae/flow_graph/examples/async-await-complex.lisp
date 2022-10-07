(begin
    (define h1 (async (+ 3 1)))
    (define h2 (async (* 3 4)))
    (+ (await h1) (await h2)))