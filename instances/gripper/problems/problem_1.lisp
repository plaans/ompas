(begin 
  (def-objects
    '(bedroom kitchen living_room no_place room)
    '(b1 b2 b3 b4 no_ball ball)
    '(left right gripper))
  (def-initial-state
    (map '(
      (at-robby living_room)
      ((at b1) bedroom)
      ((at b2) bedroom)
      ((at b3) bedroom)
      ((at b4) bedroom)
      ((carry left) no_ball)
      ((carry right) no_ball))))
)