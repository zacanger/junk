; i think these are already a part of the language, but, whatever.
(define (curry f a) (λ (b) (apply f (cons a (list b)))))
(define (commpose f g) (λ (a) (f apply g a)))
(define (flip f) (λ (a b) (f b a)))
