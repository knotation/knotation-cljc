(ns org.knotation.state)

(def rdf (partial apply str "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
(def rdfs (partial apply str "http://www.w3.org/2000/01/rdf-schema#"))
(def xsd (partial apply str "http://www.w3.org/2001/XMLSchema#"))
(def owl (partial apply str "http://www.w3.org/2002/07/owl#"))
(def kn (partial apply str "https://knotation.org/"))

(defn add-prefix
  [state prefix iri]
  (-> state
      (assoc-in [:env :prefix-iri prefix] iri)
      (assoc-in [:env :iri-prefix iri] prefix)
      (update-in [:env :prefix-sequence] (fnil conj []) prefix)))

(defn add-label
  [state label iri]
  (-> state
      (assoc-in [:env :label-iri label] iri)
      (assoc-in [:env :iri-label iri] label)
      (update-in [:env :label-sequence] (fnil conj []) label)))

(defn add-datatype
  [state predicate datatype]
  (-> state
      (assoc-in [:env :predicate-datatype predicate] datatype)))

(defn update-state
  [state {:keys [subject predicate object] :as quad}]
  ; TODO: make this configurable
  ; WARN: case macro requires literal values, not symbols or functions
  (case (:iri predicate)
    "http://www.w3.org/2000/01/rdf-schema#label"
    (add-label state (:lexical object) (:iri subject))

    "https://knotation.org/predicate/default-datatype"
    (add-datatype state (:iri subject) (:iri object))

    state))

(def blank-state {})

(def default-state
  (-> blank-state
      (add-prefix "rdf" (rdf))
      (add-prefix "rdfs" (rdfs))
      (add-prefix "xsd" (xsd))
      (add-prefix "owl" (owl))
      (add-prefix "kn" (kn))
      (add-label "label" (rdfs "label"))
      (add-label "type" (rdf "type"))
      (add-label "link" (kn "datatype/link"))
      (add-label "default datatype" (kn "predicate/default-datatype"))
      (add-datatype (rdf "type") (kn "datatype/link"))
      (add-datatype (kn "predicate/default-datatype") (kn "datatype/link"))))
