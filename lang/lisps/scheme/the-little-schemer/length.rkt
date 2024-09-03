(define length
 (λ (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))
