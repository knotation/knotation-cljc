(ns org.knotation.kn
  (:require [org.knotation.util :as util]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn declaration->state
  [{:keys [mode env] :as state} line]
  (if-let [[_ prefix iri]
           (re-matches #"@prefix (\w+):\s+<(\S+)>\s*" line)]
    (if (= mode :data)
      state
      (st/add-prefix state prefix iri))
    (util/throw-exception "Not a @prefix line")))

(defn subject->state
  [{:keys [env] :as state} line]
  (if-let [[_ subject] (re-matches #": \s*(.*)\s*" line)]
    (assoc-in state [:subject] (ln/subject->node env subject))
    (util/throw-exception "Not a subject line")))

(defn statement->state
  [{:keys [mode env graph subject] :as state} line]
  (if-let [[_ predicate-link content] (re-matches #"([^@:].*): (.*)" line)]
    (let [predicate-iri (ln/predicate->iri env predicate-link)
          predicate {:iri predicate-iri}
          datatype (get-in env [:iri-datatype predicate-iri])
          object (ob/string->object env datatype content)
          quad {:graph graph
                :subject subject
                :predicate predicate
                :object object}
          state (if (= mode :data) state (st/update-state state quad))]
      (if (= mode :env) state (assoc state :quads [quad])))
    (util/throw-exception "Not a statement:" line)))

(defn line->state
  [state line]
  (let [state (st/update-location state line)]
    (case (first line)
      nil state
      \@ (declaration->state state line)
      \: (subject->state state line)
      (statement->state state line))))

(defn processor
  [input]
  (fn [states]
    (->> input
         :lines
         ; TODO: group indented lines
         (reductions
          line->state
          (-> states
              last
              (merge input)
              (dissoc :lines :quads)
              (assoc :line-number 0)))
         (concat states))))
