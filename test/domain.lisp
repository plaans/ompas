(begin
    (def-command process (:params (?m machine) (?p package)))
    (def-command-om-model process (:params (?m machine) (?p package)) (:body nil))

    (def-command pick (:params (?r robot)))
    (def-command-pddl-model process (:params (?r robot)) (:pre-conditions true) (:effects nil))

    (def-task t_process_package (:params (?p package)))
    (def-method m_process_to_do_r
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
                    (t_process_package ?p))))
    (def-state-function robot.coordinates (:params (?r robot)) (:result (tuple int int)))
    (def-lambda go_random (lambda (?r ?l ?u)
                                (let ((x (rand-int-in-range ?l ?u))
                                      (y (rand-int-in-range ?l ?u)))
                                      (navigate_to ?r x y))))
)