(define insertR*
  (lambda (new old l) (cond ((null? l) '()))
    ((atom? (car l))
     (cond ((eq? (car l) old)
            (cons new (insertR* new old (cdr l))))) ; clearly i have given up on formatting
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
