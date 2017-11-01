(ns org.knotation.nq
  (:require [org.knotation.link :as ln]
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
   "."))

(defn states->lines
  [states]
  (->> states
       (mapcat :quads)
       (map quad->line)))
