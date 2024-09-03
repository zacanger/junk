(ns md-editor.core
    (:require [reagent.core :as rg]))

(defn editor [content]
  [:div.text
   [:textarea
    {:rows 30
     :cols 40
     :type "text"
     :value @content
     :on-change #(reset! content (-> % .-target .-value))}]])

(defn renderer [content]
  [:div.preview {:dangerouslySetInnerHTML
                 {:__html (-> content str js/md)}}])

(defn preview [content]
  (when (not-empty @content)
    [renderer @content]))

(defn ui []
  (let [content (rg/atom nil)]
    (fn []
        [:div.root
         [editor content]
         [preview content]])))

(defn mount-root
  []
  (rg/render [ui]
             (.getElementById js/document "app")))

(defn ^:export init []
  (mount-root))
