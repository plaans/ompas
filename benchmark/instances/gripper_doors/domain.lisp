(begin
  (def-types room gripper ball new_bool)
  (def-constants
    (left right gripper)
    (no_place room)
    (no_ball ball)
    (yes no new_bool)
  )
  ;state functions
  (def-state-function at-robby (:result room))
  (def-state-function at (:params (?b ball)) (:result room))
  (def-state-function carry (:params (?g gripper)) (:result ball))
  (def-state-function connected (:params (?r1 room) (?r2 room)) (:result (?result new_bool)))

  ;actions
  (def-command move (:params (?from room) (?to room)))
  (def-command-model move
    (:params (?from room) (?to room))
    (:pre-conditions 
      (= (at-robby) ?from)
      (!= ?from ?to)
      (= (connected ?from ?to) yes))
    (:effects
      (begin
          (assert 'at-robby ?to))))


  (def-command pick (:params (?obj ball) (?room room) (?gripper gripper)))
  (def-command-pddl-model pick
    (:params (?obj ball) (?room room) (?gripper gripper))
    (:pre-conditions
      (= (at ?obj) ?room)
      (= (at-robby) ?room)
      (= (carry ?gripper) no_ball))
    (:effects
      (begin
          (assert `(carry ,?gripper) ?obj)
          (assert `(at ,?obj) no_place))))

  (def-command drop (:params (?obj ball) (?room room) (?gripper gripper)))
  (def-command-pddl-model drop
    (:params (?obj ball) (?room room) (?gripper gripper))
    (:pre-conditions
      (= (carry ?gripper) ?obj)
      (= (at-robby) ?room))
    (:effects
      (begin
          (assert `(carry ,?gripper) no_ball)
          (assert `(at ,?obj) ?room ))))

  (def-task pick-and-drop (:params (?ball ball) (?room room)))

  (def-method m1
    (:task pick-and-drop)
    (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
    (:pre-conditions 
        ( = (at ?ball) (at-robby))
        (= (carry ?gripper) no_ball)
        (= ?departure (at-robby)))
        ;(= (connected ?departure ?room) yes)
    (:score 0)
    (:body
      (do
          (pick ?ball ?departure ?gripper)
          (t_move ?room)
          (drop ?ball ?room ?gripper))))
  
  (def-method m2
    (:task pick-and-drop)
    (:params (?ball ball) (?room room) (?gripper gripper) (?departure room) (?intermediaire room))
    (:pre-conditions
      (!= (at ?ball) (at-robby))
      (= (carry ?gripper) no_ball)
      (= ?departure (at-robby))
      (= ?intermediaire (at ?ball)))
    (:score 0)
    (:body
      (do
          (t_move ?intermediaire)
          (pick ?ball ?intermediaire ?gripper)
          (t_move ?room)
          (drop ?ball ?room ?gripper))))


  (def-task t_move (:params (?to room)))
  (def-method m_already_there
    (:task t_move)
    (:params (?to room))
    (:pre-conditions (= (at-robby) ?to))
    (:score 2)
    (:body true))

  ; (def-method m_connected
  ;     '((:task t_move)
  ;       (:params (?to room))
  ;       (:pre-conditions 
  ;         (check (= (connected (at-robby) ?to) yes)))
  ;       (:score 1)
  ;       (:body (move (at-robby) ?to))))

  (def-method m_recursive
    (:task t_move)
    (:params (?to room) (?intermediaire room))
    (:pre-conditions
      (!= (at-robby) ?to)
      (= (connected (at-robby) ?intermediaire) yes))
    (:score 0)
    (:body 
      (do 
          (move (at-robby) ?intermediaire)
          (t_move ?to)
          )))

)