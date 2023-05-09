(begin
    (def-duration
        (give_support_to_person 15)
        (clear_location 5)
        (inspect_person 20)
        ;move
        (fly 15)
        (inspect_location 5)
        (transfer 2)
        (replenish_supplies 4))

    (def-objects 
        (w1 w2 wheeled_robot)
        (r1 robot))

    (def-facts
        ((loc r1) (4 5))
        ((loc w1) (1 1))
        ((loc w2) (5 5))
        ((has_medicine r1) 0)
        ((has_medicine w1) 1)
        ((has_medicine w2) 1)
        ((robot_type r1) wheeled)
        ((robot_type w1) wheeled)
        ((status r1) unk)
        ((status p1) unk)
        ((status (1 1)) has_debri)
        ((real_status r1) ok)
        ((real_status p1) ok)
        ((real_status (1 1)) has_debri)
    )

    (add-task-to-execute get_supplies r1)
)