; clojure doesn't have this, weird

(defn ** [a n]
  (reduce * (repeat n a)))
