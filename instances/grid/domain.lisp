(begin


    (def-types location new_bool truck)
    (def-constants '(yes no new_bool))
    (def-state-function connected '(?x location) '(?y location) '(?r new_bool))
    (def-state-function at '(?t truck) '(?l location))
    (def-action drive '(?t truck) '(?to location))
    
    
    (def-action-model drive
        '((:params (?t truck) (?to location))
          (:pre-conditions (check (= (connected (at ?t) ?to) yes)))
          (:effects
                (assert `(at ,?t) ?to))))
    

    (def-task t_move '(?t truck) '(?to location))
    (def-method m_already_there
        '((:task t_move)
          (:params (?t truck) (?to location))
          (:pre-conditions (check (= (at ?t) ?to)))
          (:score 0)
          (:body true)))

    (def-method m_connected
        '((:task t_move)
          (:params (?t truck) (?to location))
          (:pre-conditions (and-cond (!= (at ?t) ?to) (= (connected (at ?t) ?to) yes )))
          (:score 0)
          (:body (drive ?t ?to))))

    (def-method m_recursive
        '((:task t_move)
          (:params (?t truck) (?to location) (?intermediaire location))
          (:pre-conditions (and-cond
            (!= (at ?t) ?to)
            (!= ?to ?intermediaire)
            (= (connected (at ?t) ?to) no)
            (= (connected (at ?t) ?intermediaire) yes)))
          (:score 0)
          (:body 
            (do 
                (drive ?t ?intermediaire)
                (t_move ?intermediaire ?to)))))
)