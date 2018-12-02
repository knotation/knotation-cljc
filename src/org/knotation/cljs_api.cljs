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
            [org.knotation.info :as info]))

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

(defn read-strings
  "Given a format keyword, an initial state (or nil for the default state), and 
   a sequence of inputs (strings), read each input in order, accumulating state,
   and return a sequence of states."
   [input-format initial-state inputs]
   (->> inputs
    (reductions
      (fn [previous-states input]
        (read-string input-format (last previous-states) input))
      [(or initial-state st/default-state)])
    rest 
    (mapcat identity)))

(defn read-input
  "Given a format keyword, an initial state (or nil for the default state), and 
   and input thing (either a string or a collection of strings), return a lazy
   sequence of states."
  [input-format initial-state thing]
  (cond
    (string? thing)
    (read-string input-format initial-state thing)
    (and (coll? thing) (every? string? thing))
    (read-strings input-format initial-state thing)
    :else 
    (util/throw-exception (str "Unable to read input type: " (type thing)))))

; Render Output

(defn render-states
  "Given a format keyword, an environment (or nil), and a sequence of state
   maps, return a sequence of rendered states."
  [fmt initial-state states]
  (let [initial-state (or initial-state st/default-state)]
    (case fmt
      :kn (kn/render-states initial-state states)
      :ttl 
      (->> states
           st/sequential-blank-nodes
           (ttl/render-states (get initial-state ::en/env en/default-env)))
      (util/throw-exception "Unable to read input format: " fmt))))

(defn render-output
  "Given a format keyword, an environment (or nil), and a sequence of state
   maps, return the string output of the states."
  [fmt initial-state states]
    (case fmt
      :md
      (string/join "\n"
        (reduce
          (fn [v s]
            (conj v (info/markdown (info/help s)))) [] states))
      :html
      (string/join "\n"
        (reduce
          (fn [v s]
            (conj v (info/html (info/help s)))) [] states))
      (st/render-output-string (render-states fmt initial-state states))))
