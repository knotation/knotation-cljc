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

(defn update-env
  [env {:keys [prefix iri ::rdf/si ::rdf/sb ::rdf/pi ::rdf/oi ::rdf/ol] :as state}]
  (cond
    (and prefix iri)
    (en/add-prefix env prefix iri)

    ; TODO: make this configurable
    ; WARN: case macro requires literal values, not symbols or functions
    pi
    (case pi
      "http://www.w3.org/2000/01/rdf-schema#label"
      (en/add-label env ol si)

      "https://knotation.org/kn/default-datatype"
      (en/set-datatype env si oi)

      "https://knotation.org/kn/default-language"
      (en/set-language env si ol)

      "https://knotation.org/kn/template-content"
      (en/set-template-content env si ol)

      ;else
      env)

    si
    (assoc env ::rdf/si si)

    sb
    (assoc env ::rdf/sb sb)

    :else
    env))

(def blank-state
  {::event ::blank
   ::en/env {}})

(def default-state
  (assoc blank-state ::en/env en/default-env))
