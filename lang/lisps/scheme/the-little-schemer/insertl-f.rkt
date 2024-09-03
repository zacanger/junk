(define insertl-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) ('()))
        ((test? (car l) old)
          (cons new (cons old (cdr l))))
        (else (cons (car l)
          ((insertl-f test?) new old
            (cdr l))))))))))
