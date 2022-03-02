(def-task t_process_packages)
(def-lambda '(async_process_package
        (lambda (?p) (async (t_process_package ?p)))))
(def-method m_process_initial_packages
        '((:task t_process_packages)
        (:params)
        (:pre-conditions true)
        (:effects nil)
        (:score 0)
        (:body 
            (begin
                (define list_packages (packages))
                (mapf async_check_battery list_packages)
                (t_process_new_packages list_packages)))))

    (def-task t_process_new_packages ?l)
    (def-method m_check_oncoming_packages
        '((:task t_process_new_packages)
        (:params (?l list))
        (:pre-conditions true)
        (:effects nil)
        (:score 0)
        (:body 
            (begin
                (monitor `(> ,(len ?l) (len (packages))))
                (define new_lp (packages))
                (define l_new_p (sublist (new_lp) (len ?l)))
                (mapf async_check_battery l_new_p)
                (t_check_batteries_new_robots new_lp)))))