(ns org.knotation.kn-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.kn :as kn]))

(s/def ::line string?)
(s/def ::parse-map (s/map-of keyword? string?))

(s/fdef kn/parse-map
        :args (s/cat :parse ::st/parse)
        :ret ::parse-map)

(s/fdef kn/parse-line
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-blank
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-comment
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-declaration
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-subject
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-indented
        :args (s/cat :line ::line)
        :ret ::st/parse)
(s/fdef kn/parse-statement
        :args (s/cat :line ::line)
        :ret ::st/parse)

(s/fdef kn/merge-indented-statement
        :args (s/cat :states ::st/states)
        :ret ::st/states)
(s/fdef kn/merge-indented
        :args (s/cat :states ::st/states)
        :ret ::st/states)

(s/fdef kn/read-parse
        :args (s/cat :state ::st/state)
        :ret ::st/states)
(s/fdef kn/read-blank
        :args (s/cat :state ::st/state)
        :ret ::st/state)
(s/fdef kn/read-comment
        :args (s/cat :state ::st/state)
        :ret ::st/state)
(s/fdef kn/read-prefix
        :args (s/cat :state ::st/state)
        :ret ::st/state)
(s/fdef kn/read-subject
        :args (s/cat :state ::st/state)
        :ret ::st/state)
(s/fdef kn/read-statement
        :args (s/cat :state ::st/state)
        :ret ::st/states)

(s/fdef kn/read-object
        :args (s/cat :env ::en/env :parse ::st/parse :predicate ::rdf/predicate :language (s/nilable ::rdf/language) :datatype (s/nilable ::rdf/datatype))
        :ret map?)

(s/fdef kn/render-state
        :args (s/cat :state ::st/state)
        :ret ::st/state)
(s/fdef kn/render-blank
        :args (s/cat :state ::st/state)
        :ret ::st/parse)
(s/fdef kn/render-comment
        :args (s/cat :state ::st/state)
        :ret ::st/parse)
(s/fdef kn/render-prefix
        :args (s/cat :state ::st/state)
        :ret ::st/parse)
(s/fdef kn/render-subject
        :args (s/cat :state ::st/state)
        :ret ::st/parse)
(s/fdef kn/render-statement
        :args (s/cat :state ::st/state)
        :ret ::st/parse)

(s/fdef kn/render-datatype
        :args (s/cat :env ::en/env :predicate ::rdf/predicate :object map?)
        :ret ::st/parse)
(s/fdef kn/render-object
        :args (s/cat :env ::en/env :object map?)
        :ret ::st/parse)

(s/fdef kn/parse-lines
        :args (s/cat :previous-state ::st/state :lines (s/coll-of string?))
        :ret ::st/states)
(s/fdef kn/read-parses
        :args (s/cat :previous-state ::st/state :states ::st/states)
        :ret ::st/states)
(s/fdef kn/read-lines
        :args (s/cat :previous-state ::st/state :lines (s/coll-of string?))
        :ret ::st/states)
(s/fdef kn/read-input
        :args (s/cat :previous-state ::st/state :input string?)
        :ret ::st/states)

(s/fdef kn/render-stanza
        :args (s/cat :previous-states ::st/states :states ::st/states)
        :ret ::st/states)
(s/fdef kn/render-states
        :args (s/cat :previous-state ::st/state :states ::st/states)
        :ret ::st/states)
