(pre-eval-expr '(begin
    (define a (quote test))
    (assert a 2)
    (define b (quote test2))
    a))

(pre-eval-expr '(begin
        (define list_packages (instances package))
        (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))))
    
(pre-eval-expr '(begin
        (define list_packages (instances package))
         list_packages))

(pre-eval-expr 
'(do
    (define list_packages (instances package))
    (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))    
    ))