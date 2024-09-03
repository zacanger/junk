(defmacro foo [s]
  (list reverse s))

; inspect a macro result
(macroexpand '(foo "stuff"))
; eval it
(eval (macroexpand '(foo "stuff")))
; or just (foo "stuff")

(defmacro bar [s]
  '(reverse s))
(macroexpand '(bar "stuff"))

; use ` to quote in macros, and ~ to get at the outer scope
(defmacro add [a b]
  `(+ ~a ~b))

; use ~@ to expand list args
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do  ~@body)))

; i guess use gensym to get your symbols instead of regular vars
; using # does this automatically.

; macros for infix math, let's see
(declare infix-helper)

(defn clean-arg [arg]
  (if (seq? arg)
    (infix-helper arg)
    arg))

(defn apply-arg [val [op arg]]
  (list op val (clean-arg arg)))

(defn infix-helper [[a & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg a) ops)))

(defmacro infix [form]
  (infix-helper form))
