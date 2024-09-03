(define / (λ (n m) (cond ((< n m) 0) (else (add1 (/ (- n m) m))))))
