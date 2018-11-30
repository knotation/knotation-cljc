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
      :kn (kn/read-lines initial-state (util/get-lines content))
      (util/throw-exception 
        (str "Unable to read input format: " input-format)))))

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
  [fmt initial-state states]
  (let [initial-state (or initial-state st/default-state)]
    (case fmt
      :kn (kn/render-states initial-state states)
      :ttl (ttl/render-states (get initial-state ::en/env en/default-env) states)
      (util/throw-exception (str "Unable to render output format: " fmt)))))

(defn render-output
  "Given a format keyword, an environment (or nil), and a sequence of state
   maps, return the string output of the states."
  [fmt initial-state states]
  (st/render-output-string (render-states fmt initial-state states)))
