(begin
  (def-objects
    (b1 b2 b3 b4 ball)
    (r1 r2 r3 r4 room))

(def-initial-state
   ((at b1) r1)
   ((at b2) r1)
    ((at b3) r1)
   ((at b4) r1)
   (at-robby r2)
   ((carry left) no_ball)
   ((carry right) no_ball)
   ((connected r1 r2) yes)
   ((connected r2 r1) yes)
    ((connected r2 r3) yes)
   ((connected r3 r2) yes)
  ((connected r2 r4) yes)
   ((connected r4 r2) yes)
   )
   
  (trigger-task pick-and-drop b1 r4)
)
;      r4
;      |
; r1 - r2 - r3