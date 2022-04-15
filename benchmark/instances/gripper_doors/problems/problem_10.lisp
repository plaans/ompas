(begin
  (def-objects
  '(b1 b2 b3 b4 ball)
  '(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 room))

(def-initial-state
 (map '(
   ((at b1) r1)
   ((at b2) r2)
   ((at b3) r3)
   ((at b4) r4)
   (at-robby r11)
   ((carry left) no_ball)
   ((carry right) no_ball)
   ((connected r1 r5) yes)
   ((connected r5 r1) yes)

   ((connected r3 r5) yes)
   ((connected r5 r3) yes)
   
   ((connected r4 r5) yes)
   ((connected r5 r4) yes)
   
   ((connected r6 r5) yes)
   ((connected r5 r6) yes)

   ((connected r6 r2) yes)
   ((connected r2 r6) yes)

   ((connected r6 r8) yes)
   ((connected r8 r6) yes)

   ((connected r6 r7) yes)
   ((connected r7 r6) yes)

   ((connected r2 r9) yes)
   ((connected r9 r2) yes)

   ((connected r9 r10) yes)
   ((connected r10 r9) yes)

   ((connected r2 r11) yes)
   ((connected r11 r8) yes)
   
   )))

   (trigger-task pick-and-drop b1 r10)

)

;This domain has the particularity have circular connections between r2 and r8 through r11

;      r4   r8 - r11
;      |    |    | 
; r1 - r5 - r6 - r2 - r9 - r10
;      |    |
;      r3   r7
