; map, implmented using reduce

(defn mapp [f coll]
  (reduce
    (fn [acc v]
        (conj acc (f v))) [] coll))
