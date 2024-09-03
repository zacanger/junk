(ns test.core
  (:require [fs]))

(defn write [n c]
  (.writeFileSync fs n c))

(write "thing" "stuff\n")
