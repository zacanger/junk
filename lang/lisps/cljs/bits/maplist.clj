; from a stack overflow answer
(defn maplist
  "Based on Common Lisp's maplist."
  [fn coll]
  (if (empty? coll) nil
    (cons (fn coll)
          (maplist fn (rest coll)))))
