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
  [states {:keys [::st/input] :as input-state}]
  (let [{:keys [::st/mode ::en/env ::columns] :as state} (last states)
        {:keys [::st/line-number ::st/lines]} input
        line (first lines)]
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
          language (get-in env [::en/predicate-language predicate-iri])
          datatype (get-in env [::en/predicate-datatype predicate-iri])
          object (ob/string->object env language datatype cell)
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
  [states {:keys [::st/input] :as input-state}]
  (let [{:keys [::st/mode ::en/env ::columns] :as state} (last states)
        {:keys [::st/line-number ::st/lines]} input
        line (first lines)
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
  [states input-state]
  (if (-> states last ::columns)
    (read-row states input-state)
    [(read-header states input-state)]))

(defn read-input-states
  [env input-states]
  (->> input-states
       (reductions
        (fn [previous current]
          (read-one-line previous current))
        [{::en/env env}])
       rest
       (mapcat identity)
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

(fm/register!
 {::fm/name :tsv
  ::fm/description "Knotation TSV format"
  ::fm/read read-input})
