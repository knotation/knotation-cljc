(ns org.knotation.hub
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]

            [org.knotation.format :as fm]))

(defmethod fm/parse-line
  :hub [fmt line]
  (println "PARSING HUB!" (str line))
  (clojure.edn/read-string line))

(defmethod fm/read-parse
  :hub [fmt env parse]
  (println "READING HUB PARSE!" (str parse))
  parse)

(defmethod fm/process-parses
  :hub [fmt parses]
  (println "PROCESSING HUB PARSES!" (str (vec (map #(dissoc % :en/env) parses))))
  parses)

(defmethod fm/render-state
  :hub [fmt env state]
  (println "RENDERING HUB STATE!")
  (when (= :statement (:event state))
    [[(str (select-keys state [:rt :gi :si :sb :pi :oi :ob :ol :di :ln :event]))]]))
