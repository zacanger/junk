(ns zac-tries-clojurescript.prod
  (:require [zac-tries-clojurescript.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
