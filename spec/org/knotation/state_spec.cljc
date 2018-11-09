(ns org.knotation.state-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.state :as st]))

(s/def ::st/event
  #{::st/default ; initial state
    ::st/input
    ::st/parse
    ::st/blank
    ::st/error
    ::st/comment
    ::st/space
    ::st/prefix
    ::st/base
    ::st/graph-start ::st/graph-end
    ::st/stanza-start ::st/stanza-end
    ::st/subject-start ::st/subject-end
    ::st/statement
    ::st/annotation
    ::st/header})

(s/def ::st/error-type keyword?)
(s/def ::st/error-message string?)
(s/def ::st/error-info (s/coll-of any?))
(s/def ::st/error (s/keys :req [::st/error-type ::st/error-message]
                          :opt [::st/error-info]))

(s/def ::st/line-number integer?)
(s/def ::st/column-number integer?)
(s/def ::st/location (s/keys :req [::st/line-number ::st/column-number]))
(s/def ::st/start ::st/location)
(s/def ::st/end ::st/location)

(s/def ::st/filename string?)
(s/def ::st/format keyword?)
(s/def ::st/content string?)

(s/def ::st/input (s/keys :req [::st/format ::st/content ::st/start ::st/end]
                          :opt [::st/filename]))
(s/def ::st/output (s/keys :req [::st/format ::st/content ::st/start ::st/end]
                           :opt [::st/filename]))

(s/def ::st/parse (s/coll-of (s/or :string string? :keyword keyword? :parse ::st/parse)))

(s/def ::st/state (s/keys :req [::st/event]
                          :opt [::st/error
                                ::st/location
                                ::en/env
                                ::st/input
                                ::st/parse
                                ::rdf/quad
                                ::st/output]))
(s/def ::st/states (s/coll-of ::st/state))

(s/fdef st/step-location
        :args (s/cat :location ::st/location)
        :ret ::st/location)

(s/fdef st/advance-location
        :args (s/cat :location ::st/location :content string?)
        :ret ::st/location)

(s/fdef st/input
        :args (s/cat :state ::st/state :format ::st/format :content (s/nilable string?))
        :ret ::st/state)

(s/fdef st/output
        :args (s/cat :state ::st/state :format ::st/format :content (s/nilable string?))
        :ret ::st/state)

(s/fdef st/render-parse
        :args (s/cat :parse ::st/parse)
        :ret (s/nilable string?))

(s/fdef st/render-output
        :args (s/cat :states ::st/states)
        :ret (s/coll-of string?))

(s/fdef st/render-output-string
        :args (s/cat :states ::st/states)
        :ret string?)

(s/fdef st/update-env
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::en/env)

(s/fdef st/update-state
        :args (s/cat :previous-state ::st/state :state ::st/state)
        :ret ::st/state)
