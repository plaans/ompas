(begin
   (def-task t_jobshop)
   (def-method m1
       (:task t_jobshop)
       (:score 0)
       (:body
           (do
               (define tasks 
                   (mapf (lambda (?p) 
                       (do
                           (define tasks (mapf (lambda (process)
                               `(t_process_on_machine ,?p 
                                   (arbitrary ',(find_machines_for_process (car process)))
                                   ;,(cadr process)
                                   ))
                               (package.processes_list ?p)))
                           (apply seq tasks)))
                   (instances package)))
               (apply par tasks))))

   (def-task t_process_on_machine (:params (?p package) (?m machine)))
   (def-task-om-model t_process_on_machine
        (:params (?p package) (?m machine))
        (:body
            (do
                (define rh (acquire ?m))
                ;(sleep 1)
                (release rh))))

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