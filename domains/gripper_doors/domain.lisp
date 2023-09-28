(begin
  (def-types location gripper ball)
  (def-objects
    (left right gripper)
    (robby location)
    (empty ball)
    (yes no new_bool))
  (def-resources robby)
  ;state functions
  (def-state-function at-robby (:params ) (:result location))
  (def-state-function pos (:params (?b ball)) (:result location))
  (def-state-function carry (:params (?g gripper)) (:result ball))
  (def-state-function connected (:params (?r1 location) (?r2 location)) (:result new_bool))

  ;actions
  (def-command move (:params (?from location) (?to location)))
  ; (def-command-pddl-model move
  ;   (:params (?from location) (?to location))
  ;   (:pre-conditions 
  ;     (= (at-robby) ?from)
  ;     (!= ?from ?to)
  ;     (= (connected ?from ?to) yes))
  ;   (:effects
  ;         ('at-robby ?to)))

  (def-command pick (:params (?obj ball) (?location location) (?gripper gripper)))
  ; (def-command-pddl-model pick
  ;   (:params (?obj ball) (?location location) (?gripper gripper))
  ;   (:pre-conditions
  ;     (= (at ?obj) ?location)
  ;     (= (at-robby) ?location)
  ;     (= (carry ?gripper) no_ball))
  ;   (:effects
  ;         ('carry ?gripper ?obj)
  ;         ('at ?obj no_place)))

  (def-command drop (:params (?obj ball) (?location location) (?gripper gripper)))
  ; (def-command-pddl-model drop
  ;   (:params (?obj ball) (?location location) (?gripper gripper))
  ;   (:pre-conditions
  ;     (= (carry ?gripper) ?obj)
  ;     (= (at-robby) ?location))
  ;   (:effects
  ;         ('carry ?gripper no_ball)
  ;         ('at ?obj ?location)))

  ; (def-task pick-and-drop (:params (?ball ball) (?location location)))

  ; (def-method m1
  ;   (:task pick-and-drop)
  ;   (:params (?ball ball) (?location location) (?gripper gripper) (?departure location))
  ;   (:pre-conditions 
  ;       ( = (at ?ball) (at-robby))
  ;       (= (carry ?gripper) no_ball)
  ;       (= ?departure (at-robby)))
  ;       ;(= (connected ?departure ?location) yes)
  ;   (:score 0)
  ;   (:body
  ;     (do
  ;         (pick ?ball ?departure ?gripper)
  ;         (t_move ?location)
  ;         (drop ?ball ?location ?gripper))))
  
  ; (def-method m2
  ;   (:task pick-and-drop)
  ;   (:params (?ball ball) (?location location) (?gripper gripper) (?departure location) (?intermediaire location))
  ;   (:pre-conditions
  ;     (!= (at ?ball) (at-robby))
  ;     (= (carry ?gripper) no_ball)
  ;     (= ?departure (at-robby))
  ;     (= ?intermediaire (at ?ball)))
  ;   (:score 0)
  ;   (:body
  ;     (do
  ;         (t_move ?intermediaire)
  ;         (pick ?ball ?intermediaire ?gripper)
  ;         (t_move ?location)
  ;         (drop ?ball ?location ?gripper))))


  ; (def-task t_move (:params (?to location)))
  ; (def-method m_already_there
  ;   (:task t_move)
  ;   (:params (?to location))
  ;   (:pre-conditions (= (at-robby) ?to))
  ;   (:score 2)
  ;   (:body true))

  ; ; (def-method m_connected
  ; ;     '((:task t_move)
  ; ;       (:params (?to location))
  ; ;       (:pre-conditions 
  ; ;         (check (= (connected (at-robby) ?to) yes)))
  ; ;       (:score 1)
  ; ;       (:body (move (at-robby) ?to))))

  ; (def-method m_recursive
  ;   (:task t_move)
  ;   (:params (?to location) (?intermediaire location))
  ;   (:pre-conditions
  ;     (!= (at-robby) ?to)
  ;     (= (connected (at-robby) ?intermediaire) yes))
  ;   (:score 0)
  ;   (:body 
  ;     (do 
  ;         (move (at-robby) ?intermediaire)
  ;         (t_move ?to)
  ;         )))

  (def-task place (:params (?b ball) (?r location)))
  (def-task go2 (:params (?r location)))

  (def-method move_and_drop
    (:task place)
    (:params (?b ball) (?r location) (?g gripper) (?p location))
    (:pre-conditions (= (pos ?b) ?p) (!= ?p robby))
    (:body
        (do
            (define r_h (acquire robby))
            (go2 ?p)
            (pick ?b ?p ?g)
            (go2 ?r)
            (drop ?b ?r ?g))))
)