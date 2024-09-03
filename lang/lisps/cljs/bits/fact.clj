(defn fact [n]
  (if (= n 0)
    1
    (fact -n 1)))
