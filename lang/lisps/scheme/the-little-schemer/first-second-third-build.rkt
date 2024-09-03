(define first (lambda (a) (cond (else (car a)))))

(define second (lambda (a) (cond (else (car (cdr a))))))

(define third (lambda (a) (car (cdr (cdr a)))))

(define build (lambda (a b) (cond (else (cons a (cons b ('())))))))
