; simplified
(defn reduce [f result coll]
  (if (not= '() coll)
    (reduce f (f result (first coll)) (rest coll))
    result))

(defn map [f coll]
  (when (not= '() coll)
    (conj
      (map f (rest coll))
      (f (first coll)))))

(defn filter [pred coll]
  (when (not= '() coll)
    (let [f (first coll) r (rest coll)]
      (if (pred f)
        (conj (filter pred r) f)
        (filter pred r)))))

; using core.async
(defn map [f in out]
  (go-loop []
           (let [val (<! in)]
           (if (nil? val)
             (close! out)
             (do (doseq [v (f val)]
                   (>! out v))
                 (when-not (impl/closed? out)
                   (recur)))))))

(defn filter [pred ch]
  (let [out (chan)]
    (go-loop []
             (let [val (<! ch)]
               (if (nil? val)
                 (close! out)
                 (do (when (pred val)
                       (>! out val))
                     (recur)))))
    out))

; use transducers
(defn map [f]
  (fn [rf]
    (fn [result el]
      (rf result (f el)))))

(defn filter [pred]
  (fn [rf]
    (fn [result el]
      (if (pred el)
        (rf result el)
        result))))
