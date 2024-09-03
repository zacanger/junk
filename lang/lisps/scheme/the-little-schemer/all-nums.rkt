(define all-nums (λ (lat) (cond ((null? lat) '()) (else (cond ((number? (car lat)) (cons (car lat) (all-nums (cdr lat)))) (else (all-nums (cdr lat))))))))
