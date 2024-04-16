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
                                       (define ?m (ctx-arbitrary 0 (quote (machine2))))
                                       nil
                                       (ctx-exec-task 0 (quote t_process_on_machine) (quote package0) ?m 1))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 1 (quote (machine0))))
                                       nil
                                       (ctx-exec-task 1 (quote t_process_on_machine) (quote package0) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 2 (quote (machine1))))
                                       nil
                                       (ctx-exec-task 2 (quote t_process_on_machine) (quote package0) ?m 6))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 3 (quote (machine3))))
                                       nil
                                       (ctx-exec-task 3 (quote t_process_on_machine) (quote package0) ?m 7))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 4 (quote (machine5))))
                                       nil
                                       (ctx-exec-task 4 (quote t_process_on_machine) (quote package0) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 5 (quote (machine4))))
                                       nil
                                       (ctx-exec-task 5 (quote t_process_on_machine) (quote package0) ?m 6)))))
                     (define _h1_
                           (async (begin
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 6 (quote (machine0))))
                                       nil
                                       (ctx-exec-task 6 (quote t_process_on_machine) (quote package1) ?m 2))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 7 (quote (machine2))))
                                       nil
                                       (ctx-exec-task 7 (quote t_process_on_machine) (quote package1) ?m 6))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 8 (quote (machine1))))
                                       nil
                                       (ctx-exec-task 8 (quote t_process_on_machine) (quote package1) ?m 3))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 9 (quote (machine5))))
                                       nil
                                       (ctx-exec-task 9 (quote t_process_on_machine) (quote package1) ?m 4))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 10 (quote (machine4))))
                                       nil
                                       (ctx-exec-task 10 (quote t_process_on_machine) (quote package1) ?m 5))
                                 (begin
                                       nil
                                       (define ?m (ctx-arbitrary 11 (quote (machine3))))
                                       nil
                                       (ctx-exec-task 11 (quote t_process_on_machine) (quote package1) ?m 4)))))
                     (await _h0_)
                     (await _h1_)))))
