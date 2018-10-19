(ns org.knotation.ttl-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.ttl :as ttl]))

(s/fdef ttl/render-iri
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret string?)

(s/fdef ttl/render-lexical
        :args (s/cat :lexical string?)
        :ret string?)

(s/fdef ttl/render-object
        :args (s/cat :env ::en/env :states ::st/states :triple ::rdf/triple)
        :ret (s/or :string string? :strings (s/coll-of string?)))

(s/fdef ttl/render-statement
        :args (s/cat :env ::en/env :states ::st/states :triple ::rdf/triple)
        :ret (s/coll-of string?))

(s/fdef ttl/render-subject
        :args (s/cat :env ::en/env :states ::st/states :subject ::rdf/subject)
        :ret (s/coll-of string?))

(s/fdef ttl/render-declaration
        :args (s/cat :state ::st/state)
        :ret ::st/state)

(s/fdef ttl/render-stanza
        :args (s/cat :env ::en/env :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/render-stanzas
        :args (s/cat :env ::en/env :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/number-output-lines
        :args (s/cat :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/render-states
        :args (s/cat :env ::en/env :states ::st/states)
        :ret ::st/states)
