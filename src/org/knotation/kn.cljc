(ns org.knotation.kn
  (:require [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn declaration->state
  [{:keys [::st/mode] :as state}]
  (if-let [[_ prefix iri]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches #"@prefix (\S+):\s+<(\S+)>\s*"))]
    (if (= mode :data)
      state
      (st/add-prefix state prefix iri))
    (assoc
     state
     ::st/error
     {::st/error-type :not-a-prefix-line}
     {::st/error-message "Not a @prefix line"})))

(defn subject->state
  [{:keys [::en/env] :as state}]
  (if-let [[_ subject]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches #": \s*(.*)\s*"))]
    (assoc state ::rdf/subject (ln/subject->node env subject))
    (assoc
     state
     ::st/error
     {::st/error-type :not-a-subject-line}
     {::st/error-message "Not a subject line"})))

(defn statement->state
  [{:keys [::st/mode ::en/env ::rdf/graph ::rdf/subject] :as state}]
  (if-let [[_ predicate-link content]
           (->> state
                ::st/input
                ::st/lines
                first
                (re-matches #"([^@:].*): (.*)"))]
    (let [predicate-iri (ln/predicate->iri env predicate-link)]
      (if (nil? predicate-iri)
        (assoc
         state
         ::st/error
         {::st/error-type :unrecognized-predicate
          ::st/error-message (str "Unrecognized predicate: " predicate-link)})
        (let [predicate {::rdf/iri predicate-iri}
              datatype (get-in env [::en/predicate-datatype predicate-iri])
              object (ob/string->object env datatype content)
              quad {::rdf/graph graph
                    ::rdf/subject subject
                    ::rdf/predicate predicate
                    ::rdf/object object}
              state (if (= mode :data) state (st/update-state state quad))]
          (if (= mode :env) state (assoc state ::rdf/quads [quad])))))
    (assoc
     state
     ::st/error
     {::st/error-type :not-a-statement}
     {::st/error-message "Not a statement"})))

(defn block->state
  [state]
  (case (->> state ::st/input ::st/lines first first)
    nil state
    \# state
    \@ (declaration->state state)
    \: (subject->state state)
    (statement->state state)))

(defn quad->lines
  [env {:keys [::rdf/predicate ::rdf/object] :as quad}]
  [(str
    (ln/node->name env predicate)
    ": "
    (if (::rdf/iri object)
      (ln/node->name env object)
      (::rdf/lexical object)))])

(defn process-output
  [{:keys [::en/env ::en/env-before
           ::rdf/quads ::st/output-line-count
           ::rdf/subject ::st/previous-subject]
    :or {output-line-count 0}
    :as state}]
  (let [lines
        (concat
         (when-not (= subject previous-subject)
           (let [line (str ": " (ln/node->name env-before subject))]
             (if (nil? previous-subject)
               [line]
               ["" line])))
         (when quads
           (mapcat (partial quad->lines env) quads)))]
    (if (> (count lines) 0)
      (assoc
       state
       ::st/output-line-count (+ output-line-count (count lines))
       ::st/output
       {::st/format :kn
        ::st/line-number (inc output-line-count)
        ::st/lines lines})
      state)))

(defn process-outputs
  [states]
  (->> states
       (reductions
        (fn [previous-state input-state]
          (-> input-state
              (assoc ::st/output-line-count
                     (get previous-state ::st/output-line-count 0)
                     ::st/previous-subject
                     (::rdf/subject previous-state))
              process-output))
        st/blank-state)
       rest))
