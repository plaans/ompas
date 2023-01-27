
(begin
  (def-objects
    (t1 t2 truck)
    (s l1 l2 l3 l4 l5 l6 e location))

  (def-facts
    ((at t1) s)
    ((at t2) s)
    ((connected s l1) yes)
    ((connected s l3) yes)
    ((connected l3 s) yes)
    ((connected l2 l1) yes)
    ((connected l1 l6) yes)
    ((connected l6 l2) yes)
    ((connected l2 l4) yes)
    ((connected l4 e) yes)
    ((connected l6 l5) yes)
    ((connected l5 e) yes)
    ((connected l6 l3) yes))

  (trigger-task t_move t1 e)
)