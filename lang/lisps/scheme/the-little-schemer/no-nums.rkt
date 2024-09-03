(define no-nums
 (λ (lat)
   (cond
    ((null? lat) '()))
   (else (cond
           ((number? (car lat))
            (no-nums (cdr lat)))
           (else (cons (car lat) (no-nums (cdr lat))))))))
