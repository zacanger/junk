(define multisubst (λ (new old lat) (cond ((null? lat) '())) (else (cond ((eq? (car lat) old) (cons new (multisubst new old (cdr lat)))) (else (cons (car lat) (multisubst new old (cdr lat))))))))
