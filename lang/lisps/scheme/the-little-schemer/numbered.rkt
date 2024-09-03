(define numbered?
 (λ (thing)
   (cond
    ((atom? thing) (number? thing))
    (else
     (and (numbered? (car thing))
       (numbered? (car (Cdr (cdr thing)))))))))
