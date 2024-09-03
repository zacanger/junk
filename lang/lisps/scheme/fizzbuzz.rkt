#!/usr/local/bin/racket -r

(define (fizzbuzz)
 ((for ([n (in-range 1 101)])
   (displayln
     (match (gcd n 15)
       [3 "fizz"]
       [5 "buzz"]
       [15 "fizzbuzz"]
       [_ n])))))

(fizzbuzz)
