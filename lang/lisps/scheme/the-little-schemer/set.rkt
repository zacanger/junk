(define set?
  (lambda (a)
    (cond
      ((null? a) #t)
      ((member? (car a) (cdr a)) #f)
      (else (set? (cdr a)))))
