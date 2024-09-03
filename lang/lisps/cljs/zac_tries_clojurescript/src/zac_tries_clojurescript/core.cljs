(ns zac-tries-clojurescript.core
 (:require [reagent.core :as r]
           [zac-tries-clojurescript.something :as s]
           [zac-tries-clojurescript.grid :as g]))

;; components

; grid (column) thing
(defn col [el]
  [:div.col el])

(defn wat [words]
  [g/row
   [g/third words]
   [g/third "wut"]
   [g/third "why are these not columns?"]])

(defn greetz [name]
  [:span.row "howdy, " name])

(defn columns [a b c]
  [:div.align-right
   [col a]
   [col b]])

(def app-state
  (r/atom
    {:links
     [{:url "http://zacanger.com"          :title "zacanger.com"}
      {:url "https://github.com/zacanger"  :title "github"}
      {:url "https://twitter.com/zacanger" :title "twitter"}
      {:url "http://blog.zacanger.com"     :title "blog"}]}))

; map over links
(defn make-links-list [links]
  [:ul.list-reset
   (for [link links]
     ^{:key link} [:li [:a {:href (:url link)} (:title link)]])])

(defn links-list []
  [col
   [make-links-list (:links @app-state)]])

;; page
(defn app []
  [:div
   [:h1 "This is a test."]
   [:h3.blink "Hi, I'm blinking."]
   [wat "wat"]
   [columns
    [:div [s/a-thing]
     [:span.row "sup"]
     [greetz "clojurescript"]]
    [links-list]]])


;; init
(defn mount-root []
  (r/render [app] (.getElementById js/document "root")))

(defn init! []
  (mount-root))
