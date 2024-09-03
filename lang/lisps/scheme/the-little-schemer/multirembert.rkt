(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) ('()))
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test?
                                (cdr lat)))))))
