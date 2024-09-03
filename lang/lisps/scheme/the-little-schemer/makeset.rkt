(define makeset
  (lambda (a)
    (cond
      ((null? a) '())
      (else
        (cons (car a)
              (makeset
                (multirember (car a)
                             (cdr a))))))))
