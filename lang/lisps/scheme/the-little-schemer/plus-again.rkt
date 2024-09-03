(define + (lambda (a b) (cond ((zero? b) a) (else (add1 (+ a (sub1 b)))))))
