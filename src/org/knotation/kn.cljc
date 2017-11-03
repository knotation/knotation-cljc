(ns org.knotation.kn
  (:require [org.knotation.util :as util]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn declaration->state
  [{:keys [mode env block] :as state}]
  (if-let [[_ prefix iri]
           (re-matches #"@prefix (\S+):\s+<(\S+)>\s*" (first block))]
    (if (= mode :data)
      state
      (st/add-prefix state prefix iri))
    (util/throw-exception "Not a @prefix line")))

(defn subject->state
  [{:keys [env block] :as state}]
  (if-let [[_ subject] (re-matches #": \s*(.*)\s*" (first block))]
    (assoc-in state [:subject] (ln/subject->node env subject))
    (util/throw-exception "Not a subject line")))

(defn statement->state
  [{:keys [mode env graph subject block] :as state}]
  (if-let [[_ predicate-link content]
           (re-matches #"([^@:].*): (.*)" (first block))]
    (let [predicate-iri (ln/predicate->iri env predicate-link)
          predicate {:iri predicate-iri}
          datatype (get-in env [:predicate-datatype predicate-iri])
          object (ob/string->object env datatype content)
          quad {:graph graph
                :subject subject
                :predicate predicate
                :object object}
          state (if (= mode :data) state (st/update-state state quad))]
      (if (= mode :env) state (assoc state :quads [quad])))
    (util/throw-exception "Not a statement:" block)))

(defn block->state
  [{:keys [block] :as state}]
  (case (first (first block))
    nil state
    \# state
    \@ (declaration->state state)
    \: (subject->state state)
    (statement->state state)))

(defn quad->lines
  [env {:keys [predicate object] :as quad}]
  [(str
    (ln/node->name env predicate)
    ": "
    (if (:iri object)
      (ln/node->name env object)
      (:lexical object)))])

(defn state->lines
  [{:keys [env quads] :as state}]
  (mapcat (partial quad->lines env) quads))

(defn subject-states->lines
  [states]
  (let [{:keys [env-before subject] :as state} (first states)]
    (->> states
         (mapcat state->lines)
         (concat [(str ": " (ln/node->name env-before subject))]))))

(defn states->lines
  [states]
  (->> states
       (filter :quads)
       (partition-by :subject)
       (map subject-states->lines)
       (interpose [""])
       (mapcat identity)))
