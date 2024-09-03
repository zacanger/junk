(define firsts
  (lambda (l)
    (cond
      ((null? l) ...)
      (else (cons ... (firsts (cdr l)))))))

(else (cons (Car (car l)) (firsts (cdr l ;; ...
