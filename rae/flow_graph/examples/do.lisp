(begin
    (define __r__ (check (= 1 2)))
    (if (err? __r__)
        __r__
        ok))