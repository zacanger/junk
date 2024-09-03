(take 10 (range 0 1000 2))
(take 10 (iterate (fn [p] (+ p 2)) 0))
(take 10 (iterate (partial + 2) 0))

; take 10 [2,4..]
(defn inf-list [a b]
  (let [diff (- b a)]
    iterate (partial + diff) a))
(take 10 (inf-list 2 4))

;;
(def data {:child {:child {:value 3} :value 2} :value 1})
(map :value (take-while identity (iterate :child data)))
; same as
(map :value (keep identity (take 10 (iterate :child data))))
; except we needed a take here, because iterate is infinite
; whereas the take-while terminated on the first nil
