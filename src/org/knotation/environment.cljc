(ns org.knotation.environment
  (:require [org.knotation.rdf :as rdf]))

(defn add-base
  [env base]
  (assoc-in env [::base] base))

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

(defn set-datatype
  [env predicate datatype]
  (-> env
      (assoc-in [::predicate-datatype predicate] datatype)))

(defn get-datatype
  [env predicate]
  (get-in env [::predicate-datatype predicate]))

(defn set-language
  [env predicate language]
  (-> env
      (assoc-in [::predicate-language predicate] language)))

(defn get-language
  [env predicate]
  (get-in env [::predicate-language predicate]))

(defn set-template-content
  [env template content]
  (-> env
      (assoc-in [::template-content template] content)))

(defn get-template-content
  [env template]
  (get-in env [::template-content template]))

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
      (add-label "link" (rdf/kn "link"))
      (add-label "default datatype" (rdf/kn "default-datatype"))
      (set-datatype (rdf/rdf "type") (rdf/kn "link"))
      (set-datatype (rdf/kn "default-datatype") (rdf/kn "link"))))
