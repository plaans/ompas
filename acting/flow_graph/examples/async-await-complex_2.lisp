(begin
    (define h1 (async 1))
    (define h2 (async 2))
    (+ (await h1) (await h2)))