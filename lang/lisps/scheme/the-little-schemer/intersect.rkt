(define intersect?
  (lambda (a b)
    (cond ((null? a) #f)
          (else (or (member? (car a) b)
            (intersect? (cdr a) b))))))


(define intersect
  (lambda (a b)
    (cond
      ((null? a) '())
      ((member? (car a) b)
        (cons (car a)
          (intersect (cdr a) b)))
       (else (intersect (car a) b)))))
