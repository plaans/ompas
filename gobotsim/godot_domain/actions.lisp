(begin
    (def-command (command process (:params (?m machine) (?p package))))
    (def-command (command pick (:params (?r robot))))
    (def-command-model pick
    '((:params (?r robot))
          (:pre-conditions true)
          (:effects nil)))

    (def-command pick_package '(?r robot) '(?p package))
    (def-command-model pick_package
        '((:params (?r robot) (?p package))
         (:pre-conditions true)
         (:effects nil)))

    (def-command place '(?r robot))
    (def-command-model place
        '((:params (?r robot))
         (:pre-conditions true)
         (:effects nil)))

    (def-command do_move '(?r robot) '(?a float) '(?s float) '(?d float))
    (def-command-model do_move
        '((:params (?r robot) (?a float) (?s float) (?d float))
         (:pre-conditions true)
         (:effects nil)))

    (def-command navigate_to '(?r robot) '(?x float) '(?y float))
    (def-command-model navigate_to
        '((:params (?r robot) (?x float) (?y float))
         (:pre-conditions true)
         (:effects nil)))

    (def-command navigate_to_cell '(?r robot) '(?cx int) '(?cy int))
    (def-command-model navigate_to_cell
        '((:params (?r robot) (?cx int) (?cy int))
         (:pre-conditions true)
         (:effects nil)))
    
    (def-command navigate_to_area '(?r robot) '(?area object))
    (def-command-model navigate_to_area
        '((:params (?r robot) (?area object))
         (:pre-conditions true)
         (:effects nil)))

    (def-command go_charge '(?r robot))
    (def-command-model go_charge
        '((:params (?r robot))
          (:pre-conditions true)
          (:effects nil)))


    (def-command do_rotation '(?r robot) '(?a float) '(?w float))
    (def-command-model do_rotation
        '((:params (?r robot) (?a float) (?w float))
         (:pre-conditions true)
         (:effects nil)))
    
    (def-command face_belt '(?r robot) '(?b belt) '(?w float))
    (def-command-model face_belt
        '((:params (?r robot) (?b belt) (?w float))
         (:pre-conditions true)
          (:effects nil)))

    (def-command rotate_to '(?r robot) '(?a float) '(?w float))
    (def-command-model rotate_to
        '((:params (?r robot) (?a float) (?w float))
         (:pre-conditions true)
         (:effects nil)))
)