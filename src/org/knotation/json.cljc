(ns org.knotation.json
  (:require [clojure.string :as string]
            [clojure.set]
            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.link :as ln]
            [org.knotation.object :as ob]
            [org.knotation.omn :as omn]
            [org.knotation.format :as fm]))

(defn output-lines
  [state lines]
  (assoc
   state
   ::st/output
   {::st/format :tree.json
    ::st/lines lines}))

(defn predicate-tree
  [predicate triples]
  (->> triples
       (filter #(-> % ::rdf/predicate ::rdf/iri (= predicate)))
       (filter #(-> % ::rdf/subject ::rdf/iri))
       (mapcat
        (fn [{:keys [::rdf/subject ::rdf/object]}]
         (if (and (::rdf/bnode object)
                  (-> object ::rdf/pairs second ::rdf/predicate ::rdf/iri (= (rdf/owl "intersectionOf"))))
           (->> object
                ::rdf/pairs
                second
                ::rdf/object
                ::rdf/pairs
                omn/branch->list
                (map ::rdf/iri)
                (remove nil?)
                (map (fn [o] [o (::rdf/iri subject)])))
           [[(::rdf/iri object) (::rdf/iri subject)]])))
       (remove nil?)
       (reduce
        (fn [coll [parent child]]
          (update coll parent (fnil conj []) child))
        {})))

(def class-icon "") ; "owl-icon glyphicon glyphicon-th-large")
(def individual-icon "") ; "owl-icon glyphicon glyphicon-record")

; WARN: This uses naive recursion and could blow the stack.

(defn render-subject
  [env types subclasses root]
  (merge
   {"text"
    (or (get-in env [::en/iri-label root])
        (ln/iri->curie env root)
        root)
    "icon"
    (if (contains? (set (get types (rdf/owl "Class"))) root)
      class-icon
      individual-icon)}
   (when (or (get subclasses root) (get types root))
     (let [children (concat (get subclasses root)
                            (get types root))]
       {"nodes"
        (map (partial render-subject env types subclasses) children)}))))

(defn render-graph
  [env triples]
  (let [types (predicate-tree (rdf/rdf "type") triples)
        subclasses (predicate-tree (rdf/rdfs "subClassOf") triples)]
    [{"text" "owl:Class"
      "icon" class-icon
      "nodes"
      (->> subclasses
           vals
           (mapcat identity)
           set
           (clojure.set/difference (set (keys subclasses)))
           (map (partial render-subject env types subclasses)))}]))

(defn render-states
  [states]
  (let [{:keys [::en/env] :as state} (last states)
        quads (mapcat ::rdf/quads states)]
    ; TODO: graph support
    (concat
     states
     [(->> quads
           (filter #(nil? (::rdf/graph %)))
           (render-graph env)
           util/edn->json
           vector
           (output-lines state))])))

(fm/register!
 {::fm/name :tree.json
  ::fm/description "Bootstrap Treeview JSON format"
  ::fm/render render-states})
