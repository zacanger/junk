(define leftmost
  (λ (a)
     (cond
       ((atom? (car a)) (car a))
       (else (leftmost (car a))))))
