
(begin
    (define ?t ?t)
    (define ?to ?to)
    (define ?intermediaire ?intermediaire)
    (do
        (begin
            (define ?t ?t)
            (define ?to ?to)
            (define ?intermediaire ?intermediaire)
                (do
                    ;(do
                    ;    (check (instance ?t truck))
                    ;    (check (instance ?to location))
                    ;    (check (instance ?intermediaire location)))
                    (do
                        (check (!= (read-state 'at ?t) ?to))
                        (check (= (read-state 'connected (read-state 'at ?t) ?intermediaire) yes)))
                    ))
        (do
            (exec-command 'drive ?t ?intermediaire)
            (exec-command 't_move ?t ?to))))