(ns org.knotation.kn
  (:require [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn line->state
  [{:keys [env graph subject] :as state} line]
  (or
   (when-let [[_ prefix iri] (re-matches #"@prefix (\w+):\s+<(\S+)>\s*" line)]
     (st/add-prefix state prefix iri))

   (when-let [[_ subject] (re-matches #": \s*(.*)\s*" line)]
     (assoc-in state [:subject] (ln/subject->node env subject)))

   (when-let [[_ predicate object] (re-matches #"([^@:].*): (.*)" line)]
     (let [predicate-iri (ln/predicate->iri env predicate)
           datatype (get-in env [:iri-datatype predicate-iri])
           object-node (ob/string->object env datatype object)]
       (assoc
        state
        :quads
        [{:graph graph
          :subject subject
          :predicate {:iri predicate-iri}
          :object object-node}])))

   state))

(defn processor
  [input]
  (fn [states]
    (->> input
         :lines
         ; TODO: group indented lines
         (reductions line->state (last states))
         (concat states))))
