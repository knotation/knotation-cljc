(ns org.knotation.tsv
  (:require [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn header->state
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

(defn cell->state
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

(defn row->state
  [{:keys [::st/mode ::en/env ::columns] :as state}]
  (let [line (->> state ::st/input ::st/lines first)
        cells (string/split line #"\t")
        pairs (map vector columns cells)
        subject-cell (->> pairs (filter #(-> % first ::subject?)) first second)
        subject (ln/subject->node env subject-cell)
        state (assoc state ::rdf/subject subject ::rdf/quads [])]
    (reduce cell->state state pairs)))

(defn block->state
  [{:keys [::columns] :as state}]
  (if columns
    (row->state state)
    (header->state state)))
