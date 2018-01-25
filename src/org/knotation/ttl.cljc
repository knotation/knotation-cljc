(ns org.knotation.ttl
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]
            [org.knotation.omn :as omn]))

(defn indent
  [line]
  (if (re-find #"^  " line)
    (string/replace line #"^  " "    ")
    line))

(defn render-node
  [env {:keys [::rdf/iri ::rdf/bnode ::rdf/lexical] :as node}]
  (cond
    iri (ln/iri->curie-or-wrapped-iri env iri)
    bnode bnode
    lexical (ob/object->turtle-object env node)))

(defn render-quad
  [env {:keys [::rdf/predicate ::rdf/object] :as quad}]
  (cond
    (->> object ::rdf/pairs first ::rdf/predicate ::rdf/iri (= (rdf/rdf "first")))
    (concat
     [(str "  " (render-node env predicate) " (")]
     (->> object
          ::rdf/pairs
          omn/branch->list
          (mapcat
           (fn [{:keys [::rdf/pairs] :as o}]
             (if pairs
               (->> pairs
                    (mapcat (partial render-quad env))
                    (map indent)
                    (util/surround "  [" "  ]"))
               [(str "  " (render-node env o))])))
          (map indent))
     ["  ) ;"])

    (::rdf/pairs object)
    (concat
     [(str "  " (render-node env predicate) " [")]
     (->> object
          ::rdf/pairs
          (mapcat (partial render-quad env))
          (map indent))
     ["  ] ;"])

    :else
    (string/split
     (->> [predicate object]
          (remove nil?)
          (map (partial render-node env))
          (util/surround " " ";")
          (string/join " "))
     #"\n"
     -1)))

(defn output-lines
  [state lines]
  (assoc
   state
   ::st/output
   {::st/format :ttl
    ::st/lines lines}))

(defn render-state
  [{:keys [::st/mode ::st/event ::en/env
           ::st/comment ::st/prefix ::rdf/subject]
    :as state}]
  (case (if (= :env mode) nil event)
    ::st/comment
    (output-lines state [comment])

    ::st/prefix
    (output-lines
     state
     [(str "@prefix " prefix ": <" (get-in env [::en/prefix-iri prefix]) "> .")])

    ::st/space
    (output-lines state [""])

    ::st/subject-start
    (output-lines state [(render-node env subject)])

    ::st/statement
    (output-lines state (render-quad env state))

    state))

(defn terminator
  "Replace the final semi-colon of the final line for a stanza
   with a period."
  [states]
  (let [states (vec states)
        index (->> states
                   (map-indexed vector)
                   reverse
                   (filter #(= ::st/statement (::st/event (second %))))
                   first
                   first)
        lines (get-in states [index ::st/output ::st/lines])]
    (if lines
      (assoc-in
       states
       [index ::st/output ::st/lines]
       (concat
        (butlast lines)
        [(clojure.string/replace (last lines) #";$" ".")]))
      states)))

(defn render-subject
  [states]
  (->> states
       (map render-state)
       terminator))

(defn render-states
  [states]
  (->> states
       (partition-by ::rdf/subject)
       (map render-subject)
       (mapcat identity)
       fm/number-output-lines))

(fm/register!
 {::fm/name :ttl
  ::fm/description "Turtle format"
  ::fm/render render-states})
