; *
(define x
 (λ (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (x n (sub1 m)))))))
