(define (eval e env)
  (cond
    ((symbol? e) (cadr (assq e env)))
    ((eq? (car e) 'lambda) (cons e env))
    (else (apply (eval (car e) env) (eval (cadr e) env)))))

(define (apply f x)
  (eval (cddr (car f)) (cons (list (cadr (car f)) x) (cdr f))))

(display (eval (read) '())) (newline)
