(define insert-g
  (lambda (s)
    (lambda (n o l)
      (cond
        ((null? l) ('()))
        ((eq? (car l) o)
          (s n o (car l)))
        (else (cons (car l)
          ((insert-g s) n o
            (cdr l)))))))))

;; which means we can redo insertl and insertr

(define insertl (insert-g sl))
(define insertr (insert-g sr))


;; or

(define insertl (insert-g (lambda (new old l) (cons new (cons old l)))))

;; hey how about subst

(define seq-s (lambda (new old l) (cons new l)))
(define subst (insert-g (seq-s))
