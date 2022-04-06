
(begin
  (def-objects
  '(t1 t2 truck)
  '(s l1 l2 l3 l4 e location))

  (def-initial-state
  (map '(
    ((at t1) s)
    ((at t2) s)
    ((connected s l1) yes)
    ((connected l1 s) yes)
    ((connected l1 l2) yes)
    ((connected l2 l4) yes)
    ((connected l2 e) yes)
    ((connected s l3) yes)
    ((connected l3 s) yes)
  )))

  (trigger-task t_move t1 e)
)