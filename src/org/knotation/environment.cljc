(ns org.knotation.environment
  (:require [org.knotation.rdf :as rdf]))

(defn add-prefix
  [env prefix iri]
  (-> env
      (assoc-in [::prefix-iri prefix] iri)
      (assoc-in [::iri-prefix iri] prefix)
      (update ::prefix-seq (fnil conj []) prefix)))

(defn add-label
  [env label iri]
  (-> env
      (assoc-in [::label-iri label] iri)
      (assoc-in [::iri-label iri] label)
      (update ::label-seq (fnil conj []) label)))

(defn add-datatype
  [env predicate datatype]
  (-> env
      (assoc-in [::predicate-datatype predicate] datatype)))

(def blank-env {})

(def default-env
  (-> blank-env
      (add-prefix "rdf" (rdf/rdf))
      (add-prefix "rdfs" (rdf/rdfs))
      (add-prefix "xsd" (rdf/xsd))
      (add-prefix "owl" (rdf/owl))
      (add-prefix "kn" (rdf/kn))
      (add-label "label" (rdf/rdfs "label"))
      (add-label "type" (rdf/rdf "type"))
      (add-label "link" (rdf/kn "datatype/link"))
      (add-label "default datatype" (rdf/kn "predicate/default-datatype"))
      (add-datatype (rdf/rdf "type") (rdf/kn "datatype/link"))
      (add-datatype (rdf/kn "predicate/default-datatype") (rdf/kn "datatype/link"))))
