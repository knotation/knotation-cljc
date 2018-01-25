(ns org.knotation.nq
  (:require [clojure.string :as string]
            [org.knotation.util :as util :refer [throw-exception]]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn split-quad
  [line]
  (if-let [[_ s p o g] (re-matches #"(\S+) (\S+) (.*?) (\S+)?\s*." line)]
    {::line line ::g g ::s s ::p p ::o o}
    (throw-exception (str "Could not parse quad: " line))))

(defn read-quad
  [line]
  (if-let [[_ s p o g] (re-matches #"(\S+) (\S+) (.*?) (\S+)?\s*." line)]
    {::rdf/graph (when g {::rdf/iri (ln/wrapped-iri->iri nil g)})
     ::rdf/subject (ln/wrapped-iri-or-bnode->node s)
     ::rdf/predicate {::rdf/iri (ln/wrapped-iri->iri nil p)}
     ::rdf/object (ob/nquads-object->object o)}))

(defn read-state
  [{:keys [::st/mode ::st/input] :as state}]
  (->> state
       ::st/input
       ::st/lines
       first
       read-quad
       (merge
        state
        {::st/event ::st/statement})))

(defn read-input-states
  [env input-states]
  (->> input-states
       (reductions
        (fn [previous current]
          (read-state
           (merge
            (select-keys previous [::en/env ::rdf/graph ::rdf/subject])
            current)))
        {::en/env env})
       rest
       fm/insert-graph-events
       fm/insert-subject-events))

(defn read-input
  [env
   {:keys [::st/mode ::st/line-number ::st/lines]
    :or {line-number 1}
    :as input}]
  (let [input (-> input
                  (select-keys [::st/format ::st/source])
                  (assoc ::st/format :nq))]
    (->> lines
         (map-indexed
          (fn [i line]
            (assoc
             input
             ::st/line-number (+ line-number i)
             ::st/lines [line])))
         (map (fn [input] (merge {::st/input input} (when mode {::st/mode mode}))))
         (read-input-states env))))

(defn render-node
  [{:keys [::rdf/iri ::rdf/bnode ::rdf/lexical] :as node}]
  (cond
    iri (ln/iri->wrapped-iri nil iri)
    bnode bnode
    lexical (ob/object->nquads-object node)))

(defn render-quad
  [{:keys [::rdf/graph ::rdf/subject ::rdf/predicate ::rdf/object] :as quad}]
  (str
   (->> [subject predicate object graph]
        (remove nil?)
        (map render-node)
        (string/join " "))
   " ."))

(defn render-state
  [{:keys [::st/mode ::st/event] :as state}]
  (case (if (= :env mode) nil event)
    ::st/statement
    (->> state
         rdf/unbranch-quad
         (map render-quad)
         (assoc {::st/format :nq} ::st/lines)
         (assoc state ::st/output))

    state))

(defn render-states
  [states]
  (->> states
       (map render-state)
       fm/number-output-lines))

(fm/register!
 {::fm/name :nq
  ::fm/description "N-Quads format"
  ::fm/read read-input
  ::fm/render render-states})
