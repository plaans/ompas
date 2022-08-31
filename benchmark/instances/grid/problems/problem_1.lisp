
(begin
  (def-objects
  (t1 t2 truck)
  (s l1 l2 e location))

  (def-initial-state
    ((at t1) s)
    ((at t2) s)
    ((connected s l1) yes)
    ((connected s l2) yes)
    ((connected l1 e) yes)
    ((connected l2 e) yes))

  (trigger-task t_move t1 e)
)