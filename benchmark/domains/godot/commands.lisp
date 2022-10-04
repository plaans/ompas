(begin
    (def-command process (:params (?m machine) (?p package)))

    (def-command pick (:params (?r robot)))
    
    (def-command pick_package (:params (?r robot) (?p package)))

    (def-command place (:params (?r robot)))

    (def-command do_move (:params (?r robot) (?a float) (?s float) (?d float)))

    (def-command navigate_to (:params (?r robot) (?x float) (?y float)))

    (def-command navigate_to_cell (:params (?r robot) (?cx int) (?cy int)))

    (def-command navigate_to_area (:params (?r robot) (?area object)))

    (def-command go_charge (:params (?r robot)))

    (def-command do_rotation (:params (?r robot) (?a float) (?w float)))
    
    (def-command face_belt (:params (?r robot) (?b belt) (?w float)))

    (def-command rotate_to (:params (?r robot) (?a float) (?w float)))
)