(begin
    (def-types toy)
    (def-objects (t1 t2 toy))
    (def-resources t1 t2)
    (def-command c_test (:params (?t toy) (?t2 toy)))
    (def-task test)
    (def-method m_test
        (:task test)
        (:params (?t toy))
        (:pre-conditions true)
        (:body
            (do
                (define ?t2 (arbitrary (instances toy)))
                (check (!= ?t ?t2))
                (c_test ?t ?t2)
            )
        )
    )

    (plan test)
    (exit 0)
)