#!/usr/bin/env lumo

(js/require "process")

(.setEncoding js/process.stdin "utf8")

(def sin (atom ""))

(defn greet [name]
  (println (str "sup " name)))

(println "who tf are you")

(.on js/process.stdin "data"
     (fn [data]
       (swap! sin #(str % data))))

(.on js/process.stdin "end"
     (fn []
       (swap! sin (fn [s]
                    (subs s 0 (dec (count s)))))
       (greet @sin)))
