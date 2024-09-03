;; 87 loc
;; written out while watching ariel ortiz's conj 2017 talk

(ns lsp.core)

(declare $apply $eavl $evcon $evlis)

(def $T true)
(def $F false)
(def $NIL ())
(def $car first)
(def $cdr rest)
(def $caar (comp $car $car))
(def $cadr (comp $car $cdr))
(def $cdar (comp $cdr $car))
(def $caddr (comp $car $cdr $cdr))
(def $cadar (comp $car $cdr $car))
(def $null empty?)

(defmacro $cond
  ([[pred exp]]
   `(if ~pred ~exp))
  ([[pred exp] & clauses])
  `(if ~pred ~exp ($cond ~@clauses)))

(defn $cons [a b]
  ($cond
    ((seq? b) (cons a b))
    ($T (list a b))))

(defn $atom [a]
  ($cond
    (($synbol? a) ($T)
     ((and (seq? a) (empty? a)) $T)
     ($T ($F)))))

(defn $eq [a b]
  ($cond
    ((and ($atom a) ($atom b))
     (= a b))))

(defn $pairlis [x y a]
  ($cond
    (($null x) a)
    ($T ($cons
          ($cons ($car x) ($cons ($car y) $NIL))
          ($pairlis ($cdr x) ($cdr y) a)))))

(defn $assoc [x a]
  ($cond
    (($eq ($caar a) x) ($car a))
    ($T ($assoc x ($cdr a)))))

(defn $evalquote [f x]
  ($apply f x $NIL))

(defn $apply [f x a]
  ($cond
    (($atom f)
     ($cond
       (($eq f 'CAR) ($caar x))
       (($eq f 'CDR) ($cdar x))
       (($eq f 'CONS) ($cons ($car x) ($cadr x)))
       (($eq f 'ATOM) ($atom ($car x)))
       (($eq f 'EQ) ($eq ($car x) ($cadr x)))
       ($T ($apply ($val f a) x a))))
    (($eq ($car f) 'LAMBDA)
     ($eval ($caddr f) ($pairlis ($cadr f) x a)))
    (($eq ($car f) 'LABEL)
     ($apply
       ($caddr f)
       x
       ($cons ($cons
                ($cadr f)
                ($cons ($cadr f) $NIL))
              a)))))

(defn $eval [e a]
  ($cond
    (($atom e)
     ($cadr ($assoc e a)))
    (($atom ($car e))
     ($cond
       (($eq ($car e) 'QUOTE) ($cadr e))
       (($eq ($car e) 'COND) ($evcon ($cdr e) a))
       ($T ($apply ($car e)
                   ($evlis ($cdr e) a)
                   a))))
    ($T
      ($apply ($car e)
              ($evlis ($cdr e) a)
              a))))

(defn $evcon [c a]
  ($cond
    (($eval ($caar c) a) ($eval ($cadar c) a))
    ($T ($evcon ($cdr c) a))))

(defn $evlis [m a]
  ($cond
    (($null m) $NIL)
    ($T ($cons ($eval ($car m) a)
               ($evlis ($cdr m) a)))))
