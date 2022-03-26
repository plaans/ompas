
(begin
  (def-objects
  '(t1 t2 truck)
  '(l1 l2 l3 l4 location))

(def-initial-state
 (map '(
   ((at t1) l1)
   ((at t2) l1)
   ((connected l1 l2) yes)
   ;((connected l1 l3) yes)
   ((connected l2 l4) yes))))
   ;((connected l3 l4) yes))))
)