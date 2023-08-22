(begin
   (define __r__
         (begin
            (define __r__
                  (begin
                     (define a (instance ?r (quote robot)))
                     (if a
                            nil
                            (err nil))))
            (if (err? __r__)
                   __r__
                   (begin
                      (define __r__ nil)
                      (if (err? __r__)
                             __r__
                             nil)))))
   (if (err? __r__)
          __r__
          (begin
             (define d1
                   (begin
                      (define object (read-state (quote package.location) (quote package0)))
                      (read-static-state (quote location_tile) object)))
             (begin
                   (define ?r ?r)
                   (define ?dest d1)
                   (begin
                         (define __r__
                               (begin
                                  (define __r__
                                        (begin
                                           (define a (instance ?r (quote robot)))
                                           (if a
                                                  nil
                                                  (err nil))))
                                  (if (err? __r__)
                                         __r__
                                         (begin
                                            (define a (instance ?dest (quote tile)))
                                            (if a
                                                   nil
                                                   (err nil))))))
                         (if (err? __r__)
                                __r__
                                (begin
                                   (define t_r
                                         (begin
                                            (define ?r ?r)
                                            (read-state (quote robot.coordinates_tile) ?r)))
                                   (define time
                                         (/ (begin
                                               nil
                                               (define ?t2 ?dest)
                                               (read-static-state (quote travel_distance) (quote l_r) ?t2))
                                            (begin
                                               (define ?r ?r)
                                               (read-static-state (quote robot.default_velocity) ?r))))
                                   (transitive-assert time (quote robot.coordinates_tile) ?r ?dest)))))
             (assert (quote (package.location package0)) ?r)
             nil
             nil
             (begin
                   (define ?r ?r)
                   nil
                   (begin
                         (define __r__
                               (begin
                                  (define __r__
                                        (begin
                                           (define a (instance ?r (quote robot)))
                                           (if a
                                                  nil
                                                  (err nil))))
                                  (if (err? __r__)
                                         __r__
                                         [err nil])))
                         (if (err? __r__)
                                __r__
                                (begin
                                   (define t_r
                                         (begin
                                            (define ?r ?r)
                                            (read-state (quote robot.coordinates_tile) ?r)))
                                   (define time
                                         (/ nil
                                            (begin
                                               (define ?r ?r)
                                               (read-static-state (quote robot.default_velocity) ?r))))
                                   (transitive-assert time (quote robot.coordinates_tile) ?r (quote tile_25_17))))))
             (assert (quote (package.location package0)) (quote belt13)))))
