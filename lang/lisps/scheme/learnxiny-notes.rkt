#lang racket
; i think that's kinda like a shebang, but not exactly?

; Comment
;; not sure why
;;; there are sometimes more than one
;;; but i guess
;; it doesn't
; really matter

#| block comment
  still block comment
  #|
  even moar
  |#
|#

#; discarded expression

4   ; int
4.4 ; real number
1/2
; various binary, octal, hex, whatever, not too interested atm

; fn application: (fn arg1 arg2 arg3 arg4)
; data: '(a b) (note the apostrophe)

; basic math shiz
(+ 2 2)
(- 2 2)
(* 2 2)
(/ 2 2)
(expt 2 2)
(quotient 2 2)
(remainder 2 2)
(exact->inexact 1/3) ; => 0.33333333333333 -- this is kinda nifty

#t ; true
#f ; false
(not #t) ; #f
(and 0 #f (error "no"))
(or #f 0 (error "no"))

#\A ; => #\A

; Strings are [Char]
; Backslash to escape quotes and things
; Unicode supported
(string-append "Sup " "brah")
(string-ref "Foo" 0) ; -> #\F
(format "-a bar ~a baz" "foo" "quux")
(printf "yo!\n")

(define aVar 4)
aVar ; -> 4
; Unicode supported

(let ([me "zac"])
  "Anger"
 me) ; -> zac
; let is a local var, in the let
; let* is let, but you can use previous declarations in later ones
(let* ([a 2]
       [b (+ a 2)])
      (* a b))

; letrec is recursive fuctions? come back to this later

; structs are immutable
(struct human (name age location))
(define myself
  (human "zac" 27 "utah"))

myself ; -> #<human>
(human? myself) ; -> #t

; #:mutable tag says make this thing mutable
(struct whatever (foo bar) #:mutable)
; but let's not do that, i think, at least for now

(cons 1 2) ; -> '(1 . 2)
(car (cons 1 2)) ; -> 1
(cdr (cons 1 2)) ; -> 2

; lists are put together same way as pairs
(cons 1 (cons 2 (cons 3 null))) ; '(1 2 3)
; but don't have to be
(list 1 2 3)
'(1 2 3)

(cadr (list 1 2 3)) ; -> 2
(car (cdr (list 1 2 3)))

(cddr (list 1 2 3)) ; -> '(3)
(cdr (cdr (list 1 2 3)))

(caddr (list 1 2 3)) ; -> 3
(car (cdr (cdr (list 1 2 3))))

(const 1 '(2 3)) ; -> '(1 2 3)
(append '(1 2) '(3 4)) ; -> '(1 2 3 4)

; these are kind of obvious
(map foo '(1 2 3))
(map + (list 1 2 3) (list 1 2 3))
(filter even? (list 1 2 3 4))
(count even? (list 1 2 3 4))
(take (list 1 2 3 4) 2)
(drop (list 1 2 3 4) 2)

;; VECTORS are fixed-length arrays neato
; you can add them together though
#(1 2 3)
(vector append #(1 2 3) #(4 5 6))

(list->set '(1 2 3 3 2 1)) ; -> (set 1 2 3)
(set-add (set 1 2 3) 4) ; -> (set 1 2 3 4) -- new set, not mutated
(set-remove (set 1 2 3 4) 1)
(set-member? (set 1 2 3 4) 1)

(define m (hash 'a 1 'b 2 'c 3 'd 4))
(hash-ref m 'c) ; -> 3
; default value for missing keys
(hash-ref m 'z 0)
; extending, same concept
(define n (hash-set m 'e 5))
(hash-remove m 'e)

;; THE IMPORTANT BIT (functions)
(lambda () "heyo") ; same as
(λ () "heyo")
((lambda () "hi")) ; this calls it, too

(define say-hi (lambda () "hi"))
(say-hi) ; "hi"
; with sugar
(define (say-hi-again) "hi again")
; () is for args
(define greetz
 (lambda (name)
  (string-append "howdy, " name)))
(greetz "zac")
; equivalently
(define (greets name)
 (string-append "howdy, " name))

; case-lambda : variadic fns
(define greetings
 (case-lambda
  [() "hello world"]
  [(name) (string-append "hello " name)]))
(greetings "zac") ; "hello zac"
(greetings) ; "hello world"

; default paramaters
(define (yo [name "z"])
 (string-append "hello " name))

; "packing" arguments
(define (count-args . args)
 (format "~a args passed: ~a" (length args) args))
; again, this is the same as
(define count-arguments
 (lambda args
  (format "~a args passed: ~a" (length args) args)))

; packed and unpacked (normal) args can be mixed
(define (anotherthing name . args)
 (format "sup ~a, you got ~a args" name (length args)))
; you can also throw keywords in there, but i won't mess with this just yet.

(= 4 4.0) ; #t
; eq? refers to things in mem
(eq? '() '()) ; #t
(eq? (list 4) (list 4)) ; #f
(eq? 'a 'a) ; #t
(eq? 4 4) ; #t
(eq? 4 4.0) ; #f
; but use = for numbers

; eqv and eq are the same for most types, just not numbers and characters
; equal also works with strings, pairs, vectors, hashes, and a few other things

(if #t
 "foo"   ; true (if)
 "bar")  ; false (else)

(cond [(> 2 2) (error "bro")]
      [(> 4 4) error "yo"]
      [ else 'k]) ; k

; a thing that prints out what the result for that num would be in fizzbuzz
(define (fizzbuzz-checker? n)
 (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_          #f]))
(fizzbuzz-checker? 15)
(fizzbuzz-checker? 6)

; loops: recursion
(define (loop i)
 (when (< i 10)
    (printf "i=~a\n" i)
    (loop add1 i)))

(for ([i 10])) ; do things
(for ([i (in-range 0 10)])) ; do things
(for ([i in-list]))
; also in-vector, in-string, in-set, in-hash
; many more things with loops, mixing, whatever, will come back later
; also comprehensions

; exceptions
(with-handlers ([exn:fail?])) ; stuff
; also exn:break?
; and probably others
; raise to throw

; set! to set, mutably, gross
; same with remove!

(module mything basedon/thisthing
    (provide thething-iprovide)
  (all the rest of the code))

; i'm going to avoid classes for now, but it looks like you just append a % to
; your def and have super-new, class, init, define/public
; new to new up
; send to call methods

; macros
(define-syntax-rule (while a b ...)
  (let loop ()
    (when a
      b ...
      (loop))))

; contracts are a module thing that i'll check out in the future

; IO
; ports
(define out-port (open-output-file "/file"))
(displayln "contents" out-port)
(close-output-port out-port)

(define in-port (open-input-port "/file"))
(displayln (read-line in-port))
(close-input-port)

; call-with-[in|out]put-file means you don't have to explicity close it
