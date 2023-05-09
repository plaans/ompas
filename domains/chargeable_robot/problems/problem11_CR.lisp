(begin

    (def-duration 
        (put 2)
        (take 2)
        (perceive 3)
        (charge 5)
        (move 10)
        (move_to_emergency 10)
        (move_charger 5)
        (address_emergency 15)
        (wait 5))

    (def-objects
        (l1 l2 l3 l4 l5 l6 l7 l8 location)
        (o1 o2 objects)
        (r1 r2 robots))


    (def-static-facts
        ((edges l1) (l7))
        ((edges l2) (l8))
        ((edges l3) (l8))
        ((edges l4) (l8))
        ((edges l5) (l7))
        ((edges l6) (l7))
        ((edges l7) (l1 l5 l6 l8))
        ((edges l8) (l2 l3 l4 l7)))

    (def-facts
        ((loc r1) 1)
        ((charge r1) 1)
        ((load r1) empty)
        ((pos c1) l7)
        ((pos o1) 'unk)
        ((pos o2) 'unk)
        ((containers l1) nil)
        ((containers l2) ('o2))
        ((containers l3) nil)
        ((containers l4) nil)
        ((containers l5) ('o1))
        ((containers l6) nil)
        ((containers l7) nil)
        ((containers l8) nil)
        ((emergency_handling r1) false)
        ((emergency_handling r2) false)
        ((view l1) false)
        ((view l2) false)
        ((view l3) false)
        ((view l4) false)
        ((view l5) false)
        ((view l6) false)
        ((view l7) false)
        ((view l8) false)
    )

    (add-task-to-execute fetch r1 o1)
    (add-task-to-execute fetch r2 o2)
)