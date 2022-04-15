(begin


    (def-types '(ball door gripper obj))
    (def-objects '(b1 b2 b3 ball) '(d1 d2 door) '(o1 o2 obj))
    (def-task t1)
    (def-method m1
        '((:task t1)
          (:params )
          (:pre-conditions )
          (:score 0)
          (:body
          (begin
            (print "balls: " (instance ball))
            (print "doors: " (instance door))
            (print "objs: " (instance obj))
            (print "obj d1?: " (instance d1 obj))
          ))))
)