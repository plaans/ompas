(begin 
  (def-objects
    (bedroom kitchen lr room)
    (b1 b2 b3 b4 ball))
  (def-facts
    (at-robby lr)
    ((pos b1) bedroom)
    ((pos b2) kitchen)
    ((pos b3) lr)
    ((pos b4) lr)
    ((carry left) empty)
    ((carry right) empty))
  
  (def-task debug)
  (def-method m_debug
    (:task debug)
    (:pre-conditions true)
    (:body
      (pick b3 lr right))
  )

  ;(exec-task debug)

  ;(exec-task place b1 bedroom)
  ;(exec-task place b2 bedroom)
  (exec-task place b3 bedroom)
  ;(exec-task place b4 bedroom)
)