(begin
  (def-types location new_bool truck)
  (def-constants (yes no new_bool))
  (def-state-function connected (:params (?x location) (?y location)) (:result new_bool))
  (def-state-function at (:params (?t truck)) (:result location))
  (def-action drive (:params (?t truck)) (:result location))
  
  (def-command-pddl-model drive (:params (?t truck) (?to location)))

  (def-command-pddl-model drive
    (:params (?t truck) (?to location))
    (:pre-conditions (= (connected (at ?t) ?to) yes))
    (:effects
          (assert `(at ,?t) ?to)))
    

  (def-task t_move (:params (?t truck) (?to location)))
  (def-method m_already_there
    (:task t_move)
    (:params (?t truck) (?to location))
    (:pre-conditions (check (= (at ?t) ?to)))
    (:score 0)
    (:body true))

  (def-method m_recursive
    (:task t_move)
    (:params (?t truck) (?to location) (?intermediaire location))
    (:pre-conditions (and-cond
      (!= (at ?t) ?to)
      (= (connected (at ?t) ?intermediaire) yes)))
    (:score 0)
    (:body 
      (do 
          (drive ?t ?intermediaire)
          (t_move ?t ?to))))
)