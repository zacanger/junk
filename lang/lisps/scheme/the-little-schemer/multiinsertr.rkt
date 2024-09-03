(define multiinsertR
  λ (new old lat)
  (cond
    ((null? lat) '()
                 (else
                   (cond
                     ((eq? (car lat) old)
                      (cons (car lat)
                            (cons new
                                  (multiinsertR new old
                                                (cdr lat))))
                      (else (cons (car lat)
                                  (multiinsertR new old
                                                (cdr lat))))))))))
