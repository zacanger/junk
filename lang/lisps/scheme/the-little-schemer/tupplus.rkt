; tup+
(define tup+ (λ (tup1 tup1) (cond ((and (null? tup1) (null? tup2)) '())) (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

; for any two, simplified
(define tup+ (λ (t1 t2) (cond ((null? t1) t2) ((null? t2) t1) (else (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))
