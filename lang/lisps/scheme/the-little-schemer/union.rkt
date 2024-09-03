(define union
  (lambda (a b)
    (cond ((null? a) b)
          ((member? (car a) b)
            (union (car a) b))
          (else (cons (Car a)
            (union (cdr a) b))))))
