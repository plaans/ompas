(begin
  (def-objects
  '(b1 b2 ball)
  '(r1 r2 room))

(def-initial-state
 (map '(
   ((at b1) r1)
   ((at b2) r2)
   (at-robby r1)
   ((connected r1 r2) yes)
   ((connected r2 r1) yes))))
)