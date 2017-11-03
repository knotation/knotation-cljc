(ns org.knotation.nq
  (:require [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]))

(defn node->nquad-string
  [{:keys [iri bnode lexical] :as node}]
  (cond
    iri (ln/iri->wrapped-iri nil iri)
    bnode bnode
    lexical (ob/object->nquads-object node)))

(defn quad->line
  [{:keys [graph subject predicate object] :as quad}]
  (str
   (->> [subject predicate object graph]
        (remove nil?)
        (map node->nquad-string)
        (clojure.string/join " "))
   " ."))

(defn states->lines
  [states]
  (->> states
       (mapcat :quads)
       (map quad->line)))

(defn parse-quad
  [line]
  (if-let [[_ s p o g] (re-matches #"(\S+) (\S+) (.*?) (\S+)?\s*." line)]
    {:graph (when g {:iri (ln/wrapped-iri->iri nil g)})
     :subject (ln/wrapped-iri-or-bnode->node s)
     :predicate {:iri (ln/wrapped-iri->iri nil p)}
     :object (ob/nquads-object->object o)}
    (throw (Exception. (str "Could not parse quad: " line)))))

(defn block->state
  [{:keys [mode env block] :as state}]
  (let [{:keys [subject] :as quad} (parse-quad (first block))]
    (assoc
     (if (= mode :data)
       state
       (-> state
           (assoc :subject subject)
           (st/update-state quad)))
     :quads (if (= mode :env) [] [quad]))))
