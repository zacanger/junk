(def j (juxt inc dec))
(j 42) ; => [43 41]

(def data
  [{:name :z :age 27}
   {:name :d :age 31}])

(map (juxt :name :age) data) ; => ([:z 27] [:d 31])

(group-by (juxt :name :age) data)
; or sort-by, or whatever
; neat
