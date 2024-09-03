(define evens-only*
  (λ (l)
     (cond
       ((null? l) (quote ()))
       ((atom? (car l))
        (cond
          ((even? (car l))
           (cons (car l)
                 (evens-only* (cdr l))))
          (else (evens-only* (cdr l)))))
       (else (cons (evens-only* (car l))
                   (evens-only* (cdr l)))))))
