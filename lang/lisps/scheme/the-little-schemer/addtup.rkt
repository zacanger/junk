(define addtup
 (λ (tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (addtup (cdr tup)))))))
