(ns md-editor.core
    (:require [reagent.core :as rg]
              [re-frame.core :as rf]))

(def debug?
  ^boolean js/goog.DEBUG)

(defn dev-setup []
  (when debug?
  (enable-console-print!)))

(defn dispatch-edit-event [e]
  (rf/dispatch [:edited e]))

(rf/reg-event-db
  :init-state
  (fn [_ _]
      {:edited "## this is a test\n\n* of\n* markdown\n\n```\nin clojurescript\n```\n"}))

(rf/reg-event-db
  :edited
  (fn [old new]
      (assoc old :edited new)))

; (rf/reg-sub
  ; :edited
  ; (fn [db _]
      ; (:edited db)))

(rf/reg-sub
  :edited
  (fn [db _]
      (-> db :edited)))

(defn res [t]
  [:div.preview
   {:dangerouslySetInnerHTML
    {:__html (.md js/window t)}}])

(defn ed []
  [:div.text
   [:textarea
    {:rows 30
     :cols 40
     :type "text"
     :value @(rf/subscribe [:edited])
     :on-change #(rf/dispatch [:edited (-> % .-target .-value)])}]])

(defn ui []
  [:div.root
   [ed]
   [res (-> @(rf/subscribe [:edited]))]])

(defn mount-root []
  (rf/clear-subscription-cache!)
  (rg/render [ui]
             (.getElementById js/document "app")))

(defn ^:export init []
  (rf/dispatch-sync [:init-state])
  (dev-setup)
  (mount-root))
