(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define one? (λ (n) (cond (else (= n 1)))))

(define one? (λ (n) (= n 1)))
