(begin
    (def-task t1 ?a ?b ?r)
    (def-method m1
        '((:task t1)
          (:params (?a int) (?b int) (?r robot))
          (:pre-conditions true)
          (:effects nil)
          (:score 0)
          (:body
            (begin
                (define x (* ?a 10))
                (define y (* ?b 20))
                (navigate_to ?r x y)))))

    (def-method m2
    '((:task t1)
      (:params (?a int) (?b int) (?r robot))
      (:pre-conditions true)
      (:effects nil)
      (:score 0)
      (:body
            (go_charge ?r))))
)