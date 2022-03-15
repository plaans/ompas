(begin
    (def-action move '(?from room) '(?to room))
    (def-action-model move
        '((:params (?from room) (?to room))
          (:pre-conditions (= (at-robby) ?from))
          (:effects
            (begin
                (assert 'at-robby ?to)))))
)