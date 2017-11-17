(ns org.knotation.tsv
  (:require [clojure.string :as string]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn make-state
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject ::columns]} line-number line]
  (merge
   {::en/env env
    ::columns columns}
   (when graph
     {::rdf/graph graph})
   (when subject
     {::rdf/subject subject})
   (when line
     {::st/input
      {::st/format :tsv
       ::st/line-number line-number
       ::st/lines [line]}})
   (when mode
     {::st/mode mode})))

(defn read-header
  [states line-number line]
  (let [{:keys [::st/mode ::en/env ::columns] :as state} (last states)]
    (assoc
     (make-state state line-number line)
     ::st/event ::st/header
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
   {:keys [::rdf/predicate ::column-number] :as column} cell]
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
          state (assoc state ::st/event ::st/statement)
          state (assoc-in state [::st/input ::st/column-number] column-number)
          state (if (= mode :data) state (st/update-state state quad))]
      (if (= mode :env) state (assoc state ::rdf/quads [quad])))))

(defn read-row
  [states line-number line]
  (let [{:keys [::st/mode ::en/env ::columns] :as state} (last states)
        cells (string/split line #"\t")
        pairs (map vector columns cells)
        subject-cell (->> pairs (filter #(-> % first ::subject?)) first second)
        subject (ln/subject->node env subject-cell)]
    (->> pairs
         (remove #(::subject? (first %)))
         (util/surround ::st/subject-start ::st/subject-end)
         (reductions
          (fn [previous pair]
            (case pair
              ::st/subject-start
              (assoc (make-state previous line-number line) ::st/event pair)
              ::st/subject-end
              (assoc (make-state previous nil nil) ::st/event pair)
              (read-cell (make-state previous line-number line)
                         (first pair) (second pair))))
          (assoc (last states) ::rdf/subject subject))
         rest)))

(defn read-one-line
  [states line-number line]
  (if (-> states last ::columns)
    (read-row states line-number line)
    [(read-header states line-number line)]))

(defn read-lines
  [state lines]
  (->> lines
       (map-indexed (fn [i line] [(inc i) line]))
       (util/surround ::st/graph-start ::st/graph-end)
       (reductions
        (fn [states item]
          (if (keyword? item)
            (-> states
                last
                (make-state nil nil)
                (dissoc ::columns ::rdf/subject)
                (assoc ::st/event item)
                vector)
            (read-one-line states (first item) (second item))))
        [state])
       rest
       (mapcat identity)))

(fm/register!
 {::fm/name :tsv
  ::fm/description "Knotation TSV format"
  ::fm/read read-lines})
