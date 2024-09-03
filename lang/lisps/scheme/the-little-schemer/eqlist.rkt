(define eqlist?
  (λ (a b)
    (cond
      ((and (null? a) (null b)) #t)
      ((and (null a) (atom? (car b))) #f)
      ((null? a) #f)
      ((and (atom? (car a)) (null? b)) #f)
      ((and (atom? (car a)) (atom? (car b)))
       (and (eqan? (car a) (car b))
            (eqlist? (cdr a) (cdr b))))
      ((atom? (car a)) #f)
      ((null? b) #f)
      ((atom? (car b)) #f)
      (else (and (eqlist? (car a) (car b))
                 (eqlist? (cdr a) (cdr b)))))))

(define eqlist?
  (λ (a b)
     (cond
       ((and (null? a) (null? b)) #t)
       ((or (null a) (null? b)) #f)
       ((and (atom? (car a))
             (atom? (car b)))
        (and (eqan? (car a) (car b))
             (eqlist? (cdr a) (cdr b))))
       ((or (atom? (car a))
            (atom? (car b))) #f)
       (else
         (and (eqlist? (car a) (car b))
              (eqlist? (cdr a) (cdr b)))))))

(define eqlist?
  (λ (a b)
     (cond
       ((and (null? a) (null? b)) #t)
       ((or (null? a) (null? b)) #f)
       (else
         (and (equal? (car a) (car b))
              (eqlist? (cdr a) (cdr b)))))))
