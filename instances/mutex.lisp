(mutex.lock __symbol__)
==> 
(begin
    (wait-on (not (locked? __symbol__)))
    (assert '(locked __symbol__) true ))
(mutex.locked? symbol)
=> (rae-get-state-variable 'locked? __symbol__)
(mutex.release symbol)
=> (retract '(locked __symbol__) true))


(defmacro mutex.lock (lambda (__symbol__)
     `(begin
        (wait-on (not (locked? ,__symbol__))
        (assert '(locked ,__symbol__) true)))))

(defmacro mutex.locked? (lambda (__symbol__)
    `(rae-get-state-variable 'locked? ,__symbol__)))

(defmacro mutex.release (lambda (__symbol__)
    `(retract '(locked ,__symbol__) true)))