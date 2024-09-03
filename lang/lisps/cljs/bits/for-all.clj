(defn for-all [as] (reduce #(and %1 %2) true as))
(for-all (map even? [1 2 3])) ; false
(for-all (map even? [2 4 6])) ; true
