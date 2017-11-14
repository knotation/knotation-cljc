(ns org.knotation.state
  (:require [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(def example-quad
  {::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/s"}
   ::rdf/predicate {::rdf/iri "https://example.com/p"}
   ::rdf/object {::rdf/lexical "o"}})

(def example-quads
  {::input {::format :knotation
            ::line-number 4
            ::lines ["ex:p: \"o\""]}
   ::en/env-before {}
   ::en/env {}
   ::rdf/quads [example-quad]
   ::output {::format :nquads
             ::line-number 1
             ::lines ["<https://example.com/s> <https://example.com/p> \"o\" ."]}})

(def example-error
  {::input {::format :kn
            ::line-number 5
            ::lines ["foo: bar"]}
   ::en/env-before {}
   ::en/env {}
   ::error {::error-message "Unrecognized predicate: foo"
            ::error-type :unrecognized-predicate}})

(defn add-prefix
  [{:keys [::en/env] :as state} prefix iri]
  (assoc state ::en/env (en/add-prefix env prefix iri)))

(defn add-label
  [{:keys [::en/env] :as state} label iri]
  (assoc state ::en/env (en/add-label env label iri)))

(defn add-datatype
  [{:keys [::en/env] :as state} predicate datatype]
  (assoc state ::en/env (en/add-datatype env predicate datatype)))

(defn update-state
  [state {:keys [::rdf/subject ::rdf/predicate ::rdf/object] :as quad}]
  ; TODO: make this configurable
  ; WARN: case macro requires literal values, not symbols or functions
  (case (::rdf/iri predicate)
    "http://www.w3.org/2000/01/rdf-schema#label"
    (add-label state (::rdf/lexical object) (::rdf/iri subject))

    "https://knotation.org/predicate/default-datatype"
    (add-datatype state (::rdf/iri subject) (::rdf/iri object))

    state))

(def blank-state {::en/env {}})

(def default-state
  (assoc blank-state ::en/env en/default-env))
