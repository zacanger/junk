(define rempick
 (λ (n lat)
  (cond
   ((zero? (sub1 n)) (cdr lat)
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))))

(define rempick
 (λ (n lat) (cond ((one? n) (cdr lat)) (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
