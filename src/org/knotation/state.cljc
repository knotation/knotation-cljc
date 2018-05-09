(ns org.knotation.state
  (:require [clojure.string :as string]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(def error-messages
  {:not-a-comment "Not a comment line"
   :not-a-prefix-line "Not a @prefix line"
   :not-a-subject-line "Not a subject line"
   :not-a-statement "Not a statement"
   :unrecognized-predicate "Unrecognized predicate:"
   :unrecognized-datatype "Unrecognized datatype:"})

(defn error
  [state error-type & info]
  (->> info
       (map str)
       (concat [(get error-messages error-type "ERROR:")])
       (string/join " ")
       (assoc
        {::error-type error-type}
        ::error-message)
       (merge (when info {::error-info info}))
       (assoc state ::event ::error ::error)))

(def example-quad
  {::event ::statement
   ::rdf/graph nil
   ::rdf/subject {::rdf/iri "https://example.com/s"}
   ::rdf/predicate {::rdf/iri "https://example.com/p"}
   ::rdf/object {::rdf/lexical "o"}})

(def example-quads
  {::event ::statement
   ::input {::format :knotation
            ::line-number 4
            ::lines ["ex:p: \"o\""]}
   ::en/env-before {}
   ::en/env {}
   ::rdf/quads [example-quad]
   ::output {::format :nquads
             ::line-number 1
             ::lines ["<https://example.com/s> <https://example.com/p> \"o\" ."]}})

(def example-error
  {::event ::statement
   ::input {::format :kn
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

(defn set-datatype
  [{:keys [::en/env] :as state} predicate datatype]
  (assoc state ::en/env (en/set-datatype env predicate datatype)))

(defn set-language
  [{:keys [::en/env] :as state} predicate language]
  (assoc state ::en/env (en/set-language env predicate language)))

(defn set-template-content
  [{:keys [::en/env] :as state} template content]
  (assoc state ::en/env (en/set-template-content env template content)))

(defn update-env
  [env {:keys [prefix iri si sb pi oi ol] :as state}]
  (cond
    (and prefix iri)
    (en/add-prefix env prefix iri)

    ; TODO: make this configurable
    ; WARN: case macro requires literal values, not symbols or functions
    pi
    (case pi
      "http://www.w3.org/2000/01/rdf-schema#label"
      (en/add-label env ol si)

      "https://knotation.org/predicate/default-datatype"
      (en/set-datatype env si oi)

      "https://knotation.org/predicate/default-language"
      (en/set-language env si ol)

      "https://knotation.org/predicate/template-content"
      (en/set-template-content env si ol)

      ;else
      env)

    si
    (assoc env :si si)

    sb
    (assoc env :sb sb)

    :else
    env))

(def blank-state
  {::event ::blank
   ::en/env {}})

(def default-state
  (assoc blank-state ::en/env en/default-env))
