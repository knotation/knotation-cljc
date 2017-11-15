(ns org.knotation.state-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.state :as st]))

(s/def ::st/mode keyword?)
(s/def ::st/format keyword?)
(s/def ::st/source string?)
(s/def ::st/line-number number?)
(s/def ::st/lines (s/coll-of string?))
(s/def ::st/error-type keyword?)
(s/def ::st/error-message string?)

(s/def ::st/input
  (s/keys :req [::st/format ::st/line-number ::st/lines]
          :opt [::st/source]))
(s/def ::st/error
  (s/keys :req [::st/error-type ::st/error-message]))
(s/def ::st/output
  (s/keys :req [::st/format ::st/line-number ::st/lines]
          :opt [::st/source]))

(s/def ::st/state
  (s/keys :req [::en/env]
          :opt [::st/mode ::st/input ::st/output
                ::en/env-before
                ::rdf/graph ::rdf/subject
                ::rdf/quads ::st/error]))
(s/def ::st/input-state
  (s/keys :req [::st/input ::en/env]
          :opt [::st/mode ::st/output
                ::en/env-before
                ::rdf/graph ::rdf/subject
                ::rdf/quads ::st/error]))
(s/def ::st/output-state
  (s/keys :req [::st/input ::en/env]
          :opt [::st/mode
                ::en/env-before
                ::rdf/graph ::rdf/subject
                ::rdf/quads ::st/error
                ::st/output]))

(s/def ::st/states (s/coll-of ::st/state))

(s/fdef st/add-prefix
        :args (s/cat :state ::st/state :prefix string? :iri string?)
        :ret ::st/state)

(s/fdef st/add-label
        :args (s/cat :state ::st/state :label string? :iri ::rdf/iri)
        :ret ::st/state)

(s/fdef st/add-datatype
        :args (s/cat :state ::st/state :predicate ::rdf/iri ::datatype ::rdf/iri)
        :ret ::st/state)

(s/fdef st/update-state
        :args (s/cat :state ::st/state :quad ::rdf/quad)
        :ret ::st/state)
