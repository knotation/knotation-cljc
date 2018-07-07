(ns org.knotation.hub
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]

            [org.knotation.format :as fm]))

(defmethod fm/parse-line
  :hub [fmt line]
  (println "PARSING HUB!")
  ;; (println "  -- " (str line))
  (clojure.edn/read-string line))

(defmethod fm/read-parse
  :hub [fmt env parse]
  (println "READING HUB PARSE!")
  ;; (println "  -- " (str parse))
  parse)

(defmethod fm/process-parses
  :hub [fmt parses]
  (println "PROCESSING HUB PARSES!")
  ;; (println "  -- " (str (vec (map #(dissoc % :en/env) parses))))
  parses)

(defmethod fm/process-states
  :hub [fmt states]
  (println "PROCESSING HUB STATES!")
  ;; (println "  -- " (str (vec (map #(dissoc % :en/env) states))))
  states)

(defmethod fm/expand-state
  :hub [fmt env state]
  (println "  EXPANDING HUB STATE!")
  ;; (println "    -- " (str (dissoc state :en/env)))
  [state []])

(defmethod fm/render-state
  :hub [fmt env state]
  (println "  RENDERING HUB STATE!")
  ;; (println "    -- " (keys state))
  ;; (println "    -- " (dissoc state :org.knotation.environment/env))
  [[(str (select-keys state [:rt :gi :si :sb :pi :oi :ob :ol :di :ln :event]))]])

(defmethod fm/render-states
  :hub [fmt env states]
  (println "RENDERING HUB STATE SEQUENCE!")
  (->> states
       (filter #(= :statement (:event %)))
       (map #(assoc % :output {:parse (fm/render-state fmt (or (:en/env %) env) %)}))))

;; (render-to :hub (read-from :hub "{:event :statement :gi \"http://example.com/graph\" :pi \"http://example.com/predicate\" :si \"http://example.com/subject\" :ol \"An object\"}" ))
