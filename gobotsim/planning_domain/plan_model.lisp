(begin
    (def-task t_jobshop)
    (def-method m1
        (:task t_jobshop)
        (:score 0)
        (:body
            (do
                (define list_packages (instances package))
                (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))
                list-h
                (mapf await list-h)
            )))

    

   (def-task t_process_package (:params (?p package)))
   
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