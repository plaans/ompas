(begin
    (def-command (command process (:params (?m machine) (?p package))))
    (def-task (task t_process_package (:params (?p package))))
    (def-method (method m_process_to_do_r
        (:task t_process_package)
        (:params (?p package))
        (:pre-conditions (check (!= (package.processes_list ?p) nil)))
        (:score 0)
        (:body
            (do
                (define ?m
                    (arbitrary (find_machines_for_process
                            (caar (unzip (package.processes_list ?p))))))
                    (t_process_on_machine ?p ?m)
                    (t_process_package ?p)))))
)