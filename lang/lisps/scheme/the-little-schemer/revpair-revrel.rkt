(define revpar (lambda (a) (build (second a) (first a))))

(define revrel (lambda (a) (cond ((null? a) ('())) (else (cons (revpar (car a)) (revrel (cdr rel)))))))
