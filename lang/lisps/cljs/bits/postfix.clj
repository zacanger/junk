(defn postfix [& words]
  (reduce #(if (fn? %2)
             (let [[l r & m] %]
               (cons (%2 r l) m))
             (cons %2 %))
          []
          words))
