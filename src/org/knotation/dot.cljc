(ns org.knotation.dot
  (:require [clojure.string :as string]
            [clojure.set]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.format :as fm]))

(defn output-lines
  [state lines]
  (assoc
   state
   ::st/output
   {::st/format :dot
    ::st/lines
    (concat
     ["digraph g {"
      "  graph [rankdir=BT];"
      "  node [shape=record];"
      ""]
     (map (partial str "  ") lines)
     ["}"])}))

(defn render-link
  [env {:keys [::rdf/subject ::rdf/predicate ::rdf/object] :as quad}]
  (str
   "\""
   (ln/iri->name env (::rdf/iri subject))
   "\" -> \""
   (ln/iri->name env (::rdf/iri object))
   "\" [label=\""
   (ln/iri->name env (::rdf/iri predicate))
   "\"];"))

(defn render-states
  [states]
  (let [{:keys [::en/env] :as state} (last states)
        subject-iris (->> states
                          (map ::rdf/subject)
                          (map ::rdf/iri)
                          (remove nil?)
                          distinct)
        subject-set (set subject-iris)
        quads (->> states
                   (filter #(contains? subject-set (-> % ::rdf/object ::rdf/iri)))
                   (remove #(-> % ::rdf/predicate ::rdf/iri (util/starts-with? (rdf/kn "predicate/")))))]
    (concat
     states
     [(output-lines
       state
       (concat
        (map (partial render-link env) quads)))])))

(fm/register!
 {::fm/name :dot
  ::fm/description "GraphViz DOT format"
  ::fm/render render-states})
