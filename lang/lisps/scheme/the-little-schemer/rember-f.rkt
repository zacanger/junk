(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) ('()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
        (rember-f test? a
          (cdr l)))))))


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) ('()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
          ((rember-f test?) a
            (cdr l))))))))
