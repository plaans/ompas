(begin
    (define h1 (async e1))
    (define h2 (async e2))
    (await h1)
    (await h2))