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
             (sleep 1))))