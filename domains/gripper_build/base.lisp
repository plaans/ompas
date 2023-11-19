(begin
    (define gripper-build-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_build"))
    (set-current-dir gripper-build-path)
    (read "../gripper_multi/base.lisp")

    ; New types
    (def-types
        (toy toypart carriable)
        (arm leg head torso toypart))
    ; New const location
    (def-objects (ether location))

    ; New state functions
    (def-function right_arm_of (:params (?t toy)) (:result arm))
    (def-function left_arm_of (:params (?t toy)) (:result arm))
    (def-function right_leg_of (:params (?t toy)) (:result leg))
    (def-function left_leg_of (:params (?t toy)) (:result leg))
    (def-function head_of (:params (?t toy)) (:result head))
    (def-function torso_of (:params (?t toy)) (:result torso))
    (def-state-function built (:params (?t toy)) (:result boolean))

    ; New commands
    (def-command assemble (:params (?r robot) (?t toy)))
    (def-command-om-model assemble
        (:params (?r robot) (?t toy))
        (:body
            (do
                (check (! (built ?t)))
                (define r_pos (at-rob ?r))
                (define check_pos
                    (lambda (?o) (check (= r_pos (pos ?o)))))
                (check_pos (right_arm_of ?t))
                (check_pos (left_arm_of ?t))
                (check_pos (right_leg_of ?t))
                (check_pos (left_leg_of ?t))
                (check_pos (head_of ?t))
                (check_pos (torso_of ?t))
                (define new_pos
                    (lambda (?o) (effect 'pos ?o 'ether)))
                (durative-effect 5 'pos ?t r_pos)
                (effect 'built ?t true)
                (new_pos (right_arm_of ?t))
                (new_pos (left_arm_of ?t))
                (new_pos (right_leg_of ?t))
                (new_pos (left_leg_of ?t))
                (new_pos (head_of ?t))
                (new_pos (torso_of ?t)))))

    (def-command dismantle (:params (?r robot) (?t toy)))
    (def-command-om-model dismantle
            (:params (?r robot) (?t toy))
            (:body
                (do
                    (define r_pos (at-rob ?r))
                    (check (built ?t))
                    (check (= (pos ?t) r_pos))
                    (define new_pos
                        (lambda (?o) (effect 'pos ?o r_pos)))
                    (effect 'built ?t false)
                    (durative-effect 5 'pos ?t 'ether)
                    (new_pos (right_arm_of ?t))
                    (new_pos (left_arm_of ?t))
                    (new_pos (right_leg_of ?t))
                    (new_pos (left_leg_of ?t))
                    (new_pos (head_of ?t))
                    (new_pos (torso_of ?t)))))
)