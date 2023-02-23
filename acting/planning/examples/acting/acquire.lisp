(begin
    (define rh (acquire r1))
    (release rh)
    (release (acquire r1))
    )