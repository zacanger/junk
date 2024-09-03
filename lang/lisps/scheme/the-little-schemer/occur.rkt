(define occur
 (λ (a lat
      (cond
        ((null? lat) 0)
        (else
         (cond
           ((eq? (car lat) a)
            (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat)))))))))
