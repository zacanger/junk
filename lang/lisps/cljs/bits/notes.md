## notes from 4th may 2017 meetup

```clojure
(def foo {:bar "a"})
(get foo :baz "default")
;; => "default"
```

* conj will do the most efficient thing for that data structure
  * for `list` it'll be at the front; for vectors it'll be at the end
  * for maps, ¯\_(ツ)_/¯

* `clj->js`!!!!!
  * `(.log js/console (clj->js {:foo "bar"}))`

```clojure
(def num-one (atom 1))
```

* CLOJURE DOES NOT HAVE CAR/CDR?!
  * `first`/`rest`
* just swap an atom. there's your state management. done.
* there's a `js->clj` also
  * use with `:keywordize-keys` to turn string keys into keywords
* also a `#js` reader, but it's shallow
* `js* "foo()"` -- eval
* `:optimizations :advanced` does DCE and mangling

```
cljs.user=> (type (list* `(1 2)))
cljs.core/Cons
cljs.user=> (type `(1 2))
cljs.core/LazySeq
cljs.user=> (type (list 1 2))
cljs.core/List
```

* defn is a macro (def fn)

```clojure
#(+ 2 %)
;; same as
(fn [a] (+ 2 a))

#(* 2 (+ %1 %2))
;; same as
(fn [a b] (+ 2 (+ a b)))

(js/console.log "hello")
;; same as
(.log js/console "hello")
```

Interop

```clojure
(clj->js {:foo "bar"})
;; same as
#js {:foo "bar"}
;; this is a 'reader literal' which i assume is a macro
```

Args

```clojure
(defn arg-printer [& args] (pr-str args))
(arg-printer 'a 'b 'c) ;; "(a b c)"
```

Nested loops

```clojure
;; in js:
;; let xs = []
;; for (let x = 0; x < 2; x++) {
;;  for (let y = 0; y < 3; y++) {
;;    xs.push([ x, y ])
;;   }
;; }
(for [x (range 2) y (range 3)] [x y])
```

```clojure
(instance? js/Object (clj->js {})) ;; => true
```
