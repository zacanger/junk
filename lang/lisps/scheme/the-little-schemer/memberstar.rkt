(define member*
  (λ (a l)
     (cond
       ((null? l) #f)
       ((atom? (car l))
        (or (eq? (car l) a)
            (member* a (cdr l))))
       (else (or (member* a (car l))
                 (member* a (cdr l)))))))
