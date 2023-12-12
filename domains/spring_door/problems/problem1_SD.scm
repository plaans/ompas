(begin
    (def-duration
        (unlatch_1 5)
        (unlatch_2 5)
        (hold_door 2)
        (pass_door 3)
        (release_door 2)
        (close_doors 3)
        (move 10)
        (take 2)
        (put 2))

    (def-objects 
        (l1 l2 l3 l4 l5 location)
        (r1 r2 robots)
        (d1 d2 d3 door))

    (def-static-facts
        ((edges l1) (l4))
        ((edges l2) (l5))
        ((edges l3) (l6))
        ((edges l4) (l1 l5))
        ((edges l5) (l2 l4 l6))
        ((edges l6) (l3 l5))
        ((door_location l1 l4) d1)
        ((door_location l2 l5) d2)
        ((door_location l3 l6) d3)
        ((door_type d1) spring)
        ((door_type d2) spring)
        ((door_type d3) spring)
        )

    (def-facts
        ((load r1) empty)
        ((load r2) empty)
        ((door_status d1) closed)
        ((door_status d2) closed)
        ((door_status d3) closed)
        ((loc r1) l6)
        ((loc r2) l5)
        ((done 0) false)
        ((door_type d1) unk)
        ((door_type d2) unk)
        ((door_type d3) unk)
    )

    (add-task-to-execute fetch r1 o1 5)
)