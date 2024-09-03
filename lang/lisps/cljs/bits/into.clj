; all these things work
(into [] (range 10))
(into #{} [1 2 3 4])
(into {} [[:a 1] [:b 2]])
; possibly performs better than conj, because
; uses transients under the hood
(into [2 3 4] (range 10)) ; like a concat
(defn union [a b] (into a b))
(union #{1 2} #{2 3}) ; => #{1 2 3}
(defn intersection [a b] (into #{} (filter a) b))
(intersection #{1 2} #{2 3}) ; => #{2}
