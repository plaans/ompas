(do
   true
   (do
         nil
         (begin
               nil
               nil
               nil
               nil
               (begin
                     (define _h0_
                           (async (begin
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine2))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 1))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine0))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine1))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 6))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine3))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 7))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine5))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine4))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package0) ?m 6)))))
                     (define _h1_
                           (async (begin
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine0))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 2))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine2))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 6))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine1))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine5))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 4))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine4))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 5))
                                 (begin
                                       nil
                                       (define ?m (arbitrary (quote (machine3))))
                                       nil
                                       (exec-task (quote t_process_on_machine) (quote package1) ?m 4)))))
                     (await _h0_)
                     (await _h1_)))))
