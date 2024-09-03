(define subset?
  (lambda (a b)
    (cond
      ((null? a) #t)
      (else
        (and (member? (car a) b)
          (subset? (cdr a) b))))))
