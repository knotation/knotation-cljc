(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn read-header
  [{:keys [::en/env block] :as state}]
  (let [line (->> state ::st/input ::st/lines first)]
    (assoc
     state
     ::columns
     (for [[index cell] (map-indexed vector (string/split line #"\t"))]
       (merge
        {::label cell
         ::column-number (inc index)}
        (if (= cell "@subject")
          {::subject? true}
          {::rdf/predicate {::rdf/iri (ln/predicate->iri env cell)}}))))))

(defn read-cell
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject] :as state}
   [column cell]]
  (if (::subject? column)
    state
    (let [predicate (::rdf/predicate column)
          predicate-iri (::rdf/iri predicate)
          datatype (get-in env [::en/predicate-datatype predicate-iri])
          object (ob/string->object env datatype cell)
          quad
          {::rdf/graph graph
           ::rdf/subject subject
           ::rdf/predicate predicate
           ::rdf/object object}
          state (if (= mode :data) state (st/update-state state quad))]
      (if (= mode :env) state (update state ::rdf/quads conj quad)))))

(defn read-row
  [{:keys [::st/mode ::en/env ::columns] :as state}]
  (let [line (->> state ::st/input ::st/lines first)
        cells (string/split line #"\t")
        pairs (map vector columns cells)
        subject-cell (->> pairs (filter #(-> % first ::subject?)) first second)
        subject (ln/subject->node env subject-cell)
        state (assoc state ::rdf/subject subject ::rdf/quads [])]
    (reduce read-cell state pairs)))

(defn read-state
  [{:keys [::columns] :as state}]
  (if columns
    (read-row state)
    (read-header state)))

(defn read-states
  [{:keys [::st/mode] :as state} lines]
  (->> lines
       (reductions
        (fn [previous current]
          (read-state
           (merge
            {::st/input
             {::st/format :kn
              ::st/line-number
              (inc (get-in previous [::st/input ::st/line-number] 0))
              ::st/lines [current]}
             ::columns (::columns previous)
             ::en/env (::en/env previous)}
            (when mode
              {::st/mode mode})
            (when-let [s (::rdf/subject previous)]
              {::rdf/subject s}))))
        (dissoc state ::columns))
       rest))

(fm/register!
 {::fm/name :tsv
  ::fm/description "Knotation TSV format"
  ::fm/read read-states})
