(begin
  (def-objects
  '(b1 b2 b3 b4 ball)
  '(r1 r2 r3 room))

(def-initial-state
 (map '(
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
   ))))