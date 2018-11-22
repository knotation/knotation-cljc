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
        :args (s/cat :env ::en/env :quad ::rdf/quad)
        :ret string?)

(s/fdef ttl/render-blank
        :args (s/cat :state ::st/state)
        :ret string?)

(s/fdef ttl/render-prefix
        :args (s/cat :state ::st/state)
        :ret string?)

(s/fdef ttl/render-base
        :args (s/cat :state ::st/state)
        :ret string?)

(s/fdef ttl/render-stanza-start
        :args (s/cat :state ::st/state)
        :ret (s/nilable string?))

(s/fdef ttl/render-stanza-end
        :args (s/cat :state ::st/state)
        :ret (s/nilable string?))

(s/fdef ttl/render-subject-start
        :args (s/cat :state ::st/state)
        :ret string?)

(s/fdef ttl/render-subject-end
        :args (s/cat :state ::st/state)
        :ret (s/nilable string?))

(s/fdef ttl/render-statement
        :args (s/cat :state ::st/state)
        :ret string?)

(s/fdef ttl/render-state
        :args (s/cat :state ::st/state)
        :ret ::st/state)

(s/fdef ttl/get-subject
        :args (s/cat :state ::st/state)
        :ret (s/nilable ::rdf/subject))

(s/fdef ttl/state-tree
        :args (s/cat :state ::st/state)
        :ret vector?)

(s/fdef ttl/annotate-statement
        :args (s/cat :coll map?)
        :ret ::st/state)

(s/fdef ttl/annotate-subject-start
        :args (s/cat :coll map?)
        :ret ::st/state)

(s/fdef ttl/annotate-subject-start
        :args (s/cat :coll map?)
        :ret ::st/state)

(s/fdef ttl/build-tree
        :args (s/cat :coll map?)
        :ret map?)

(s/fdef ttl/flatten-tree
        :args (s/cat :state-tree vector?)
        :ret ::st/states)

(s/fdef ttl/sort-statements
        :args (s/cat :grouped-states (s/map-of ::rdf/subject ::st/states)
                     :annotations (s/coll-of ::rdf/bnode)
                     :subjects (s/coll-of ::rdf/subject))
        :ret ::st/states)

(s/fdef ttl/sort-stanza
        :args (s/cat :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/render-stanza
        :args (s/cat :previous-states ::st/states :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/render-stanzas
        :args (s/cat :env ::en/env :states ::st/states)
        :ret ::st/states)

(s/fdef ttl/render-states
        :args (s/cat :env ::en/env :states ::st/states)
        :ret ::st/states)
