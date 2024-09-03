(define a-pair?
  (lambda (a)
    (cond
      ((atom? a) #f)
      ((null? a) #f)
      ((null? (cdr a)) #f)
      ((null? (cdr (cdr a))) #t)
      (else #f))))
