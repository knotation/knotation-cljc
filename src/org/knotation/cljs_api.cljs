(ns org.knotation.cljs-api
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.kn :as kn]
            [org.knotation.tsv :as tsv]
            [org.knotation.ttl :as ttl]
            [org.knotation.api :as api]))

; Read input

(defn read-string
  "Given a format keyword, an initial state (or nil for the default state), and
   a content string, return a lazy sequence of states."
  [input-format initial-state content]
  (let [initial-state (or initial-state st/default-state)]
    (case input-format
      ; TODO: more read formats
      :kn (kn/read-lines initial-state (util/split-lines content))
      :else 
      (util/throw-error 
        (format "Unable to read input format '%s'" input-format)))))

(defn read-input
  "Given a format keyword, an initial state (or nil for the default state), and 
   a thing to read from (string or collection of strings), return a lazy 
   sequence or states."
  [input-format initial-state thing]
  (let [initial-state (or initial-state st/default-state)]
    (read-string input-format initial-state thing)))

; Render Output

(defn render-states
  "Given a format keyword, an environment (or nil), and a sequence of state
   maps, return a sequence of rendered states."
  [fmt env states]
  (let [env (or env en/default-env)]
    (case fmt
      :kn (kn/render-states env states)
      :ttl (ttl/render-states env states)))
      :else 
      (util/throw-error (format "Unable to render output format '%s'" fmt)))

(defn render-output
  "Given a format keyword, an environment (or nil), and a sequence of state
   maps, return the string output of the states."
  [fmt env states]
  (let [env (or env en/default-env)]
    (st/render-output-string (render-states fmt env states))))
