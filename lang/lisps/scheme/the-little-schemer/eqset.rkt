(define eqset? (lambda (a b) (and (subset? a b) (subset? b a))))
