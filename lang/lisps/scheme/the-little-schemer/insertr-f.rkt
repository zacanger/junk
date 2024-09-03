(define insertr-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) ('()))
        ((test? (car l) old)
          (cons old (cons new (cdr l))))
        (else (cons (car l)
          ((insertr-f test?) new old
            (cdr l)))))))))
