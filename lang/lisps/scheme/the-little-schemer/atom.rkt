(define atom?
 (λ (x)
   (and (not (pair? x)) (not (null? x)))))
