(define pick
 (λ (n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else (pick (sub1 n) (cdr lat))))))
