(defn printall [a]
  (if (not (empty? a))
    (do
      (println (first a))
      (printall (rest a)))))
