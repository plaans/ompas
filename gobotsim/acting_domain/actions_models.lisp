(begin
    ;action navigate_to_cell
    ;
    (def-lambda '(dist_robot_cell (lambda (?r ?cell)
      (let* ((r_c (robot.coordinates ?r))
              (r_x (first r_c))
              (r_y (second r_c))
              (c_x (first ?cell))
              (c_y (second ?cell)))
          (sqrt (sq (- r_x c_x))(sq (- r_y c_x)))))))

    ;returns the shortest cell
    (def-lambda '(closest_cell 
      (lambda (?r cells)
          (if (null? cells)
              nil
              (let* ((shortest (closest_cell ?r (cdr cells)))
                    (cell (first cells))
                    (dist (dist_robot_cell ?r cell)))
                  (if (null? shortest)
                      (dist)
                      (if (< dist (dist_robot_cell shortest))
                          cell
                          shortest)))))))

    (def-action-operational-model navigate_to_area
      '((:params (?r robot) (?area symbol))
        (:body (do
                  (check (or 
                      (instance ?area interact_area)
                      (instance ?area parking_area)))
                  (cond 
                      ((instance ?area interact_area)
                        (assert (robot.in_interact_ares (list ?area))))
                      ((instance ?area parking_area)
                        (assert (robot.in_station ?r) true)))))))
    

    (def-action-model navigate_to
        '((:params (?r robot) (?x int) (?y int))
         (:pre-conditions true)
         (:effects nil)))

    (def-action-model rotate_to
        '((:params (?r robot) (?a float) (?s float))
         (:pre-conditions true)
         (:effects nil))))
)