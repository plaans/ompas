(begin
    (define gripper-build-path
            (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_build"))
    (set-current-dir gripper-build-path)
    (load "../gripper_multi/om.scm")

    (def-task build_toy (:params (?t toy)))
    (def-method m_build_toy_noop
        (:task build_toy)
        (:params (?t toy))
        (:pre-conditions (built ?t))
        (:body nil))

    (def-method m_build_toy
        (:task build_toy)
        (:params (?t toy))
        (:pre-conditions (! (built ?t)))
        (:body
            (do
                (define ?r (arbitrary (instances room)))
                (par
                    '(place (right_leg_of ?t) ?r)
                    '(place (left_leg_of ?t) ?r)
                    '(place (torso_of ?t) ?r)
                    '(place (right_arm_of ?t) ?r)
                    '(place (left_arm_of ?t) ?r)
                    '(place (head_of ?t) ?r))
                (define r_building (arbitrary (instances robot)))
                (define rh (acquire r_building))
                (go2 r_building ?r)
                (assemble r_building ?t))))
)