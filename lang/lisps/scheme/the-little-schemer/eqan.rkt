(define eqan? (λ (a b) (cond ((and (number? a) (number? b)) (= a b)) ((or (number? a) (number? b)) #f) (else (eq? a b)))))
