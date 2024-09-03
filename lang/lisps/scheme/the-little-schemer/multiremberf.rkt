(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) ('()))
        ((test? a (car lat))
          ((multirember-f test?) a
            (cdr lat)))
          (else (cons (car lat)
            ((multirember-f test?) a
              (cdr lat))))))))
