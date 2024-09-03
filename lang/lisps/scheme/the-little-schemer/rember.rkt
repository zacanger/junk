;; rm first instance of member

(define rember
  (λ (a lat)
     (cond
      ((null? lat) '()
                   else (cond)
                   ((eq? (car lat) a) (cdr lat))
                   (else (rember a)
                         (cdr lat))))))

;; meh. but with cons:

(define rember
  (λ (a lat)
     (cond
      ((null? lat) '()
                   (else (cond))
                   ((eq? (car lat) a) (cdr lat))
                   (else (cons (car lat))
                         (rember a)
                         (cdr lat))))))

;; meh...
(define rember
  (λ (a lat)
     (cond
      ((null? lat) '()))
   ((eq? (car lat) a) (cdr lat))
   (else (cons (car lat)
          (rember a (cdr lat))))))

(define rember
  (λ (a b)
     (cond
       ((null? b) '()))
     (else (cond
             ((equal? (car b) a) (cdr b))
             (else (cons (car b)
                         (rember a (cdr b))))))))

(define rember
  (λ (s l)
     (cond
       ((null? l) '()))
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l))))))
