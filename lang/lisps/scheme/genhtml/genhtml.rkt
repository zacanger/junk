(define-syntax define-datatype
  (syntax-rules ()
    ((define-datatype <type>   ; <type> is ignored, documentation only
       (<constructor> <args> ...) ...)
     (begin
       (%match-define-constructor <constructor> <args> ...)
       ...))))

(define-syntax %match-define-constructor
  (syntax-rules ()
    ((%match-define-constructor (<constructor> <predicate>) <args> ...)
     (begin
       (%match-define-constructor <constructor> <args> ...)
       (define <predicate>
        (let ((<c> <constructor>))
          (lambda (thing)
            (thing <c> (lambda _ #t) (lambda () #f)))))))
    ((%match-define-constructor <constructor> <args> ...)
     (define <constructor>
       (letrec ((<c>
                 (lambda (<args> ...)
                     (lambda (token win lose)
                       (if (eq? token <c>) (win <args> ...) (lose))))))
        <c>)))))

(define-syntax match
  (syntax-rules ()
    ((match <E> (<pattern> <exp>) ...)
     (let ((E <E>))
       (%match-aux E (<pattern> <exp>) ...)))))

(define-syntax %match-aux
  (syntax-rules (else)
    ((%match-aux E) (%match-no-match))
    ((%match-aux E (else <exp>)) <exp>)
    ((%match-aux E ((<constructor> <args> ...) <exp>) <rest> ...)
     (let ((fail (lambda () (%match-aux E <rest> ...))))
       (E <constructor>
        (%match-bind (<args> ...) () <exp> fail)
        fail)))))

(define-syntax %match-bind
  (syntax-rules (_)
    ((%match-bind ((<arg> ...) <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...)
      (<formals> ... temp)
      (match temp
        ((<arg> ...) <exp>)
        (else (<fail>)))
      <fail>))
    ((%match-bind (_ <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...) (<formals> ... temp) <exp> <fail>))
    ((%match-bind (<arg> <args> ...) (<formals> ...) <exp> <fail>)
     (%match-bind (<args> ...) (<formals> ... <arg>) <exp> <fail>))
    ((%match-bind () (<formals> ...) <exp> <fail>)
     (lambda (<formals> ...) <exp>))))

(define (%match-no-match) (error "match: no matching pattern"))

(define-datatype html
  ($VERBATIM item)
  ($STRING item)
  ($NEWLINE)
  ($SEQUENTIALLY:l l))

(define ($SEQUENTIALLY . l)
  ($SEQUENTIALLY:l l))

(define (print-html l)
  (match l
    (($VERBATIM item) (display item))
    (($STRING item)
     (begin
       (display "\"")
       (display item)
       (display "\"")))
    (($NEWLINE) (newline))
    (($SEQUENTIALLY:l l)
     (for-each print-html l))))

(define NIL ($SEQUENTIALLY))

(define (comment what)
  ($SEQUENTIALLY
   ($VERBATIM "<!--")
   ($VERBATIM what)
   ($VERBATIM "-->")))

(define (DTD what)
  ($SEQUENTIALLY
   ($VERBATIM "<!")
   ($VERBATIM what)
   ($VERBATIM ">")))

(define (space) ($VERBATIM " "))

(define (DOCTYPE)
  (DTD "doctype html"))

(define (attributes a)
  ($SEQUENTIALLY:l
   (map (lambda (item)
         ($SEQUENTIALLY ($VERBATIM (symbol->string (car item)))
            (if (null? (cdr item))
                NIL
                ($SEQUENTIALLY
                 ($VERBATIM "=")
                 ($STRING (cadr item))))
            (space)))
    a)))

(define (tag t)
  (lambda (body . attr)
    ($SEQUENTIALLY
      ($SEQUENTIALLY
       ($VERBATIM "<")
       ($VERBATIM t)
       ($VERBATIM " ")
       (attributes attr)
       ($VERBATIM ">"))
      body
      ($SEQUENTIALLY
       ($VERBATIM "</")
       ($VERBATIM t)
       ($VERBATIM ">")))))

(define (unbalanced-tag t)
  (lambda (body . attr)
    ($SEQUENTIALLY
      ($SEQUENTIALLY
       ($VERBATIM "<")
       ($VERBATIM t)
       ($VERBATIM " ")
       (attributes attr)
       ($VERBATIM ">"))
      body)))

(define HEAD (tag "head"))
(define HTML (tag "html lang=\"en\""))
(define TITLE (tag "title"))
(define BODY (tag "body"))
(define TABLE (tag "table"))
(define TR:l (tag "tr"))
(define (TR . l) (TR:l ($SEQUENTIALLY:l l)))
(define TD (tag "td"))
(define DIV (tag "div"))
(define STRONG (tag "strong"))
(define SUP (tag "sup"))
(define CITE (tag "cite"))
(define CODE (tag "code"))
(define A (tag "a"))
(define B (tag "b"))
(define P (tag "p"))
(define EM (tag "em"))
(define H1 (tag "h1"))
(define H2 (tag "h2"))
(define H3 (tag "h3"))
(define H4 (tag "h4"))
(define H5 (tag "h5"))
(define H6 (tag "h6"))
(define UL:l (tag "ul"))
(define (UL . l) (UL:l ($SEQUENTIALLY:l l)))
(define LI (unbalanced-tag "li"))
(define PRE:t (tag "pre"))
(define (PRE x) (PRE:t ($VERBATIM x)))

; shortcut for common case
(define (A:HREF link body)
  (A body (list 'href link)))

(define (A:NAME link body)
  (A body (list 'name link)))

(define (unbalanced-tag-no-body name)
  (lambda attr
    ($SEQUENTIALLY
     ($VERBATIM "<") ($VERBATIM name) ($VERBATIM " ")
     (attributes attr)
     ($VERBATIM ">")
     ($NEWLINE))))

(define META (unbalanced-tag-no-body "meta"))
(define LINK (unbalanced-tag-no-body "link"))
(define IMG (unbalanced-tag-no-body "img"))
(define HR (unbalanced-tag-no-body "hr"))
(define BR (unbalanced-tag-no-body "br"))

; shortcut for text
(define (TEXT . l)
  ($SEQUENTIALLY:l
   (map (lambda (thing)
         (if (string? thing)
             ($VERBATIM thing)
             thing))
    l)))
