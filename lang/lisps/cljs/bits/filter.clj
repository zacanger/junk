; filter, implmented using reduce

(defn fltr [f coll]
  (reduce
    (fn [acc v]
        (if (f v)
          (conj acc v)
          acc))
    [] coll))
