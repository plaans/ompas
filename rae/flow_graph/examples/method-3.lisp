(begin
    (define h1 (async (exec-command move r1 x1 y1)))
    (define h2 (async (exec-command move r2 x2 y2)))
    (await h1)
    (await h2))