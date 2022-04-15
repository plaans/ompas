(begin
  (def-objects
  '(b1 b2 ball)
  '(r1 r2 room))

(def-initial-state
 (map '(
   ((at b1) r1)
   ((at b2) r1)
   (at-robby r2)
   ((carry left) no_ball)
   ((carry right) no_ball)
   ((connected r1 r2) yes)
   ((connected r2 r1) yes))))

  (trigger-task pick-and-drop b1 r2)
)

;r1 - r2