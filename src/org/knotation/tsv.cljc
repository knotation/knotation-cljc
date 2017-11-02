(ns org.knotation.tsv
  (:require [org.knotation.util :as util]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn header->state
  [{:keys [env block] :as state}]
  (assoc
   state
   :columns
   (for [[index cell] (map-indexed vector (clojure.string/split (first block) #"\t"))]
     (merge
      {:label cell
       :column-number (inc index)}
      (if (= cell "@subject")
        {:subject? true}
        {:predicate {:iri (ln/predicate->iri env cell)}})))))

(defn cell->state
  [{:keys [mode env graph subject] :as state} [column cell]]
  (if (:subject? column)
    state
    (let [predicate (:predicate column)
          predicate-iri (:iri predicate)
          datatype (get-in env [:predicate-datatype predicate-iri])
          object (ob/string->object env datatype cell)
          quad
          {:graph graph
           :subject subject
           :predicate predicate
           :object object}
          state (if (= mode :data) state (st/update-state state quad))]
      (if (= mode :env) state (update state :quads conj quad)))))

(defn row->state
  [{:keys [mode env columns block] :as state}]
  (let [cells (clojure.string/split (first block) #"\t")
        pairs (map vector columns cells)
        subject-cell (->> pairs (filter #(-> % first :subject?)) first second)
        subject (ln/subject->node env subject-cell)
        state (assoc state :subject subject :quads [])]
    (reduce cell->state state pairs)))

(defn block->state
  [{:keys [columns] :as state}]
  (if columns
    (row->state state)
    (header->state state)))
