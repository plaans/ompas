(begin



    (def-task t_jobshop)
    (def-method m1
        (:task t_jobshop)
        (:score 0)
        (:body
            (do
                (define tasks (mapf (lambda (?p) `(t_process_package ,?p)) (instances package)))
                (apply par tasks)   
            )))

    

   (def-task t_process_package (:params (?p package)))
   (def-method m_process_to_do_r
       (:task t_process_package)
       (:params (?p package))
       (:pre-conditions (!(null? (package.processes_list ?p))))
       (:score 0)
       (:body
           (do
               (define tasks (mapf (lambda (process)
                    `(t_process_on_machine ,?p 
                        ;,(car process)
                        (arbitrary ',(find_machines_for_process (car process)))
                        ,(cadr process)))
                    (package.processes_list ?p)))
               (apply seq tasks)
               )))

   (def-task t_process_on_machine (:params (?p package) (?m machine) (?d float)))

   (def-task test)
    (def-method m_test
        (:task test)
        (:body
            (begin
                (define a 'test)
                (assert a 2)
                (define b 'test2)
                a)))
)