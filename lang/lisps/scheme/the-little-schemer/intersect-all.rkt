(define intersect-all
  (lambda (a)
    (cond
      ((null? (cdr a)) (car a))
      (else (intersect (car a)
        (intersect-all (cdr a)))))))
