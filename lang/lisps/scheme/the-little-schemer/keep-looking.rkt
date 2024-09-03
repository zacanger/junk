(define keep-looking
  (λ (a sorn lat)
     (cond
       ((number? sorn)
        (keep-looking a (pick sorn lat) lat))
       (else (eq? sorn a)))))
