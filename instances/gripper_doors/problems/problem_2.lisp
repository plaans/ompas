(begin
  (def-objects
  '(b1 b2 b3 b4 ball)
  '(r1 r2 r3 r4 r5 room))

(def-initial-state
 (map '(
   ((at b1) r1)
   ((at b2) r2)
    ((at b3) r3)
   ((at b4) r4)
   (at-robby r5)
   ((carry left) no_ball)
   ((carry right) no_ball)
   ((connected r1 r5) yes)
   ((connected r5 r1) yes)
    ((connected r2 r5) yes)
   ((connected r5 r2) yes)
    ((connected r3 r5) yes)
   ((connected r5 r3) yes)
    ((connected r4 r5) yes)
   ((connected r5 r4) yes)
   ))))