(define (equal?
          (λ (a b)
             (cond
               ((and (atom? a) (atom b)
                     (eqan? a b)))
               ((atom? a) #f)
               ((atom? b) #f)
               (else (eqlist? a b))))))

(define equal?
  (λ (a b)
     (cond
       ((and (atom? a) (atom? b)
             (eqan? a b)))
       ((or (atom? a) (atom b)) #f)
       (else (eqlist? a b)))))
