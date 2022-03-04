(begin
    (def-action pick '(?r robot))
    (def-action-model pick
    '((:params (?r robot))
          (:pre-conditions true)
          (:effects nil)))

    (def-action pick_package '(?r robot) '(?p package))
    (def-action-model pick_package
        '((:params (?r robot) (?p package))
         (:pre-conditions true)
         (:effects nil)))

    (def-action place '(?r robot))
    (def-action-model place
        '((:params (?r robot))
         (:pre-conditions true)
         (:effects nil)))

    (def-action do_move '(?r robot) '(?a float) '(?s float) '(?d float))
    (def-action-model do_move
        '((:params (?r robot) (?a float) (?s float) (?d float))
         (:pre-conditions true)
         (:effects nil)))

    (def-action navigate_to '(?r robot) '(?x float) '(?y float))
    (def-action-model navigate_to
        '((:params (?r robot) (?x float) (?y float))
         (:pre-conditions true)
         (:effects nil)))

    (def-action navigate_to_cell '(?r robot) '(?cx int) '(?cy int))
    (def-action-model navigate_to_cell
        '((:params (?r robot) (?cx int) (?cy int))
         (:pre-conditions true)
         (:effects nil)))
    
    (def-action navigate_to_area '(?r robot) '(?area object))
    (def-action-model navigate_to_area
        '((:params (?r robot) (?area object))
         (:pre-conditions true)
         (:effects nil)))

    (def-action go_charge '(?r robot))
    (def-action-model go_charge
        '((:params (?r robot))
          (:pre-conditions true)
          (:effects nil)))


    (def-action do_rotation '(?r robot) '(?a float) '(?w float))
    (def-action-model do_rotation
        '((:params (?r robot) (?a float) (?w float))
         (:pre-conditions true)
         (:effects nil)))
    
    (def-action face_belt '(?r robot) '(?b belt) '(?w float))
    (def-action-model face_belt
        '((:params (?r robot) (?b belt) (?w float))
         (:pre-conditions true)
          (:effects nil)))

    (def-action rotate_to '(?r robot) '(?a float) '(?w float))
    (def-action-model rotate_to
        '((:params (?r robot) (?a float) (?w float))
         (:pre-conditions true)
         (:effects nil)))
)