;; namespaces
(ns foo.core)

;; macros
(defmacro unless
  [pred & body]
  `(if (not ~pred)
    (do ~@body) nil))
;; which is
(macroexpand '(unless true (/ 1 0))
; => (if (clojure.core/not true) (do (/ 1 0)) nil)')`)

;; function calls
(+ 1 (* 2 3))

;; definition
(defn square
  [x]
  (* x x))

;; anon fn
(fn [x] (* x x))

;; js interop
(. js/document (getElementById "root"))
(.-value input)

;; let -- local binding
(let [x 1] (println x))

;; atom
(atom {})

;; channels (core.async)
(chan)
(put! "value")
(>! "value")
(take! "value")
(<! "value")
(go) ;; everything in a go block is basically a state machine
(go
  (println "waiting...")
  (<! clicks-chan)
  (println "got clikz"))

;; equality
(def a-thing {:foo 'bar'})
(def thing-two {:foo 'bar'})
(= a-thing thing-two) ;; true

;; transducers are composable transformations
;; kind of like composable ramda things without yet having a collection
(defn is-admin?
  [record]
  (= "admin" (record :role)))
(defn full-name
  [record]
  (let [first-name (record :first-name)
        last-name (record :last-name)]
        (str first-name last-name)))
(defn admin-full-names
  (comp
    (filter is-admin?)
    (map full-name)))

