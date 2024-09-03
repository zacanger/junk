(ns zac-tries-clojurescript.grid
  (:require [reagent.core :as r]))

(defn row [el]
  (:div.row el))

(defn col [el]
  (:div.col el))

(defn half [el]
  (:div.col-half el))

(defn third [el]
  (:div.col-third el))

(defn quart [el]
  (:div.col-quarter el))

(defn fifth [el]
  (:div.col-fifth el))

(defn tenth [el]
  (:div.col-tenth el))
