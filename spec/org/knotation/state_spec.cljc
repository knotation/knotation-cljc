(ns org.knotation.state-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.state :as st]))

(s/def ::st/error-type keyword?)
(s/def ::st/error-message string?)
(s/def ::st/error-info (s/coll-of any?))
(s/def ::st/error (s/keys :req [::st/error-type ::st/error-message]
                          :opt [::st/error-info]))

(s/def ::st/format keyword?)
(s/def ::st/source string?)
(s/def ::st/line-number number?)
(s/def ::st/lines (s/coll-of string?))
(s/def ::st/content string?)

(s/def ::st/input (s/keys :req [::st/format ::st/content ::st/line-number ::st/column-number]))
(s/def ::st/parse (s/coll-of (s/or :string string? :keyword keyword? :parse ::st/parse)))

(s/def ::st/event
  #{::st/blank ; default value
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

(s/def ::st/output (s/keys :req [::st/format ::st/content]
                           :opt [::st/line-number]))

(s/def ::st/state (s/keys :opt [::st/error ::en/env
                                ::st/input ::st/parse
                                ::st/event ::rdf/quad
                                ::st/output]))
(s/def ::st/states (s/coll-of ::st/state))

(s/fdef st/output
        :args (s/cat :state ::st/state :format ::st/format :content (s/nilable string?))
        :ret ::st/state)

(s/fdef st/update-env
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::en/env)

(s/fdef st/update-env
        :args (s/cat :previous-state ::st/state :state ::st/state)
        :ret ::st/state)
