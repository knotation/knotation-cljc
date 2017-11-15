(ns org.knotation.nq
  (:require [clojure.string :as string]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn read-quad
  [line]
  (if-let [[_ s p o g] (re-matches #"(\S+) (\S+) (.*?) (\S+)?\s*." line)]
    {::rdf/graph (when g {::rdf/iri (ln/wrapped-iri->iri nil g)})
     ::rdf/subject (ln/wrapped-iri-or-bnode->node s)
     ::rdf/predicate {::rdf/iri (ln/wrapped-iri->iri nil p)}
     ::rdf/object (ob/nquads-object->object o)}
    (throw (Exception. (str "Could not parse quad: " line)))))

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
     ::rdf/quads (if (= mode :env) [] [quad]))))

(defn read-states
  [{:keys [::st/mode] :as state} lines]
  (->> lines
       (reductions
        (fn [previous current]
          (read-state
           (merge
            {::st/input
             {::st/format :kn
              ::st/line-number 1
              ::st/lines [current]}
             ::en/env (::en/env previous)}
            (when mode
              {::st/mode mode})
            (when-let [s (::rdf/subject previous)]
              {::rdf/subject s}))))
        state)
       rest))

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
  [{:keys [::rdf/quads ::st/output-line-count]
    :or {output-line-count 0}
    :as state}]
  (if quads
    (assoc
     state
     ::st/output-line-count (+ output-line-count (count quads))
     ::st/output
     {::st/format :nq
      ::st/line-number (inc output-line-count)
      ::st/lines (map render-quad quads)})
    state))

(defn render-states
  [states]
  (rest
   (reductions
    (fn [previous-state input-state]
      (-> input-state
          (assoc ::st/output-line-count
                 (get previous-state ::st/output-line-count 0))
          render-state))
    st/blank-state
    states)))

(fm/register!
 {::fm/name :nq
  ::fm/description "N-Quads format"
  ::fm/read read-states
  ::fm/render render-states})
