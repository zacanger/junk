(ns graph.core)

(defrecord Graph [nodes links])

(defn add-node [graph {:keys [id] :as node}]
  (assoc-in graph [:nodes id] node))

(defn link [graph a from to b]
  (update-in graph [:links a from] (fnil conj #{}) [to b]))

(defprotocol INode
  (ingest [state port msg]))

(defn conj-vec (fnil conj []))

; (-> {} (emit :out 1) (emit :out 2))
(defn emit [state port msg]
  (update-in state [:outputs port] conj-vec msg))

; (-> (->Map :a inc) (ingest :in 1))
(defrecord Map [id f]
  INode
  (ingest [state port msg]
    (case port
      :in (emit state :out (f msg)))))

(let [a (atom 0)]
  (defn next-id []
    (swap! a inc)))

(defn map-node [f]
  (->Map (next-id) f))

;; example
(-> (->Graph nil nil)
    (add-node {:id :a})
    (add-node {:id :b})
    (add-node {:id :c})
    (link :a :out :in :b)
    (link :b :out :in :c)
    (link :c :out :in :a)) ; loop

;; weirder
(-> (->Graph nil nil)
    (add-node (->Map :a inc))
    (add-node (->Map :b #(* % %)))
    (add-node (->Map :c dec))
    (link :a :out :in :b)
    (link :b :out :in :c))
