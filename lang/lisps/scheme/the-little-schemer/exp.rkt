(define ^ (λ (n m) (cond ((zero? m) 1) (else (x n (^ n (sub1 m)))))))
