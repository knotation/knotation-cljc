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
  (let [line (first (::st/lines input))
        {:keys [::rdf/subject] :as quad} (read-quad line)]
    (assoc
     (if (= mode :data)
       state
       (-> state
           (assoc ::rdf/subject subject)
           (st/update-state quad)))
     ::st/event ::st/statement
     ::rdf/quads (if (= mode :env) [] [quad]))))

(defn make-state
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject] :as previous}
   {:keys [::line-number ::line] :as quad}]
  (merge
   {::en/env env}
   (when mode
     {::st/mode mode})
   (when graph
     {::rdf/graph graph})
   (when subject
     {::rdf/subject subject})
   (when quad
     {::st/input
      {::st/format :nq
       ::st/line-number line-number
       ::st/lines [line]}})))

(defn read-subject
  [states quads]
  (->> quads
       (util/surround ::st/subject-start ::st/subject-end)
       (reductions
        (fn [previous quad]
          (if (keyword? quad)
            (assoc (make-state previous nil) ::st/event quad)
            (read-state (make-state previous quad))))
        (assoc (last states) ::rdf/subject (->> quads first ::s (ln/subject->node nil))))
       rest))

(defn read-graph
  [states quads]
  (->> quads
       (partition-by ::s)
       (util/surround ::st/graph-start ::st/graph-end)
       (reductions
        (fn [subject-states quads]
          (if (keyword? quads)
            (-> subject-states
                last
                (make-state nil)
                (dissoc ::rdf/subject)
                (assoc ::st/event quads)
                vector)
            (read-subject subject-states quads)))
        [(merge
          (dissoc (last states) ::rdf/graph ::rdf/subject)
          (when-let [graph (->> quads first ::g (ln/graph->node nil))]
            {::rdf/graph graph}))])
       rest
       (mapcat identity)))

(defn read-lines
  [state lines]
  (->> lines
       (map-indexed vector)
       (map (fn [[i q]] (assoc (split-quad q) ::line-number (inc i))))
       (partition-by ::g)
       (reductions read-graph [state])
       rest
       (mapcat identity)))

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
  [{:keys [::st/mode ::st/event ::rdf/quads] :as state}]
  (case (if (= :env mode) nil event)
    ::st/statement
    (->> quads
         (mapcat rdf/unbranch-quad)
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
  ::fm/read read-lines
  ::fm/render render-states})
