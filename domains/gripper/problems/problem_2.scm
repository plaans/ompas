(begin 
  (def-objects
    (bedroom kitchen living_room room)
    (b1 b2 b3 b4 ball))
  (def-initial-state
    (at-robby living_room)
    ((at b1) bedroom)
    ((at b2) bedroom)
    ((at b3) bedroom)
    ((at b4) bedroom)
    ((carry left) no_ball)
    ((carry right) no_ball))
      
  (trigger-task pick-and-drop b2 living_room))