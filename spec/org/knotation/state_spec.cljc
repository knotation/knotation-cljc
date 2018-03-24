(ns org.knotation.state-spec
  (:require [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.state :as st]))

(s/def ::st/event
  #{::st/blank ; default value
    ::st/error
    ::st/comment
    ::st/space
    ::st/prefix
    ::st/graph-start ::st/graph-end
    ::st/subject-start ::st/subject-end
    ::st/statement
    ::st/header})
(s/def ::st/mode keyword?)
(s/def ::st/format keyword?)
(s/def ::st/source string?)
(s/def ::st/line-number number?)
(s/def ::st/lines (s/coll-of string?))
(s/def ::st/error-type keyword?)
(s/def ::st/error-info (s/coll-of any?))
(s/def ::st/error-message string?)

(s/def ::st/input
  (s/keys :req [::st/format ::st/line-number ::st/lines]
          :opt [::st/source]))
(s/def ::st/error
  (s/keys :req [::st/error-type ::st/error-message]
          :opt [::st/error-info]))
(s/def ::st/output
  (s/keys :req [::st/format ::st/line-number ::st/lines]
          :opt [::st/source]))

(s/def ::st/state
  (s/keys :req [::st/event ::en/env]
          :opt [::st/mode ::st/input ::st/output
                ::rdf/graph ::rdf/subject ::rdf/predicate ::rdf/object
                ::st/error]))
(s/def ::st/input-state
  (s/keys :req [::st/event ::st/input ::en/env]
          :opt [::st/mode ::st/output
                ::rdf/graph ::rdf/subject ::rdf/predicate ::rdf/object
                ::st/error]))
(s/def ::st/output-state
  (s/keys :req [::st/event ::st/input ::en/env]
          :opt [::st/mode
                ::rdf/graph ::rdf/subject ::rdf/predicate ::rdf/object
                ::st/error
                ::st/output]))

(s/def ::st/states (s/coll-of ::st/state))

(s/fdef st/add-prefix
        :args (s/cat :state ::st/state :prefix string? :iri string?)
        :ret ::st/state)

(s/fdef st/add-label
        :args (s/cat :state ::st/state :label string? :iri ::rdf/iri)
        :ret ::st/state)

(s/fdef st/set-datatype
        :args (s/cat :state ::st/state :predicate ::rdf/iri :datatype ::rdf/datatype)
        :ret ::en/env)

(s/fdef st/set-language
        :args (s/cat :state ::st/state :predicate ::rdf/iri :language ::rdf/language)
        :ret ::en/env)

(s/fdef st/update-state
        :args (s/cat :state ::st/state :quad ::rdf/quad)
        :ret ::st/state)
