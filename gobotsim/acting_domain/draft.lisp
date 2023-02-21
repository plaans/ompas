(begin
    (def-task test)
    (def-method m_test
        (:task test)
        (:body 
            (unzip (package.processes_list ?p))
        )
    )
)