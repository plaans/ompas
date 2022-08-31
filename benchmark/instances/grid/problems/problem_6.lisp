
(begin
  (def-objects
    (t1 t2 truck)
    (s l1 l2 l3 l4 l5 l6 l7 e location))

  (def-initial-state
    ((at t1) s)
    ((at t2) s)
    ((connected s l1) yes)
    ((connected l1 s) yes)
    ((connected s l3) yes)
    ((connected s l6) yes)
    ((connected l6 s) yes)
    ((connected l3 l6) yes)
    ((connected s l7) yes)
    ((connected l7 l5) yes)
    ((connected l5 l6) yes)
    ((connected l1 l2) yes)
    ((connected l2 l4) yes)
    ((connected l6 l2) yes)
    ((connected l6 e) yes)
    ((connected l7 e) yes))

  (trigger-task t_move t1 e)
)