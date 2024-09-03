(define < (λ (n m) (cond ((zero? m) #f) ((zero? n) #t) (else (< (sub1 n) (sub1 m))))))
