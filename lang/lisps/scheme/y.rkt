(define Y
  (λ (f)
     (let ((g (λ (h)
                 (λ (x) ((f (h h)) x)))))
       (g g))))
