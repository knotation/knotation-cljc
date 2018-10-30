(ns org.knotation.kn-spec
  (:require [clojure.spec.alpha :as s]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as st]
            [org.knotation.state-spec]
            [org.knotation.kn :as kn]))

(s/def ::line string?)
(s/def ::parse sequential?)
(s/def ::parse-map map?)
(s/def ::parses (s/coll-of ::parse))

(s/fdef kn/parse-map
        :args (s/cat :parse ::parse)
        :ret ::parse-map)

(s/fdef kn/parse-line
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-blank
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-comment
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-declaration
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-subject
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-indented
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-annotation
        :args (s/cat :line ::line)
        :ret ::parse)
(s/fdef kn/parse-statement
        :args (s/cat :line ::line)
        :ret ::parse)

(s/fdef kn/merge-parses
        :args (s/cat :parses ::parses)
        :ret ::parses)
(s/fdef kn/process-parses
        :args (s/cat :parses ::parses)
        :ret ::parses)

(s/fdef kn/read-parse
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-blank
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-comment
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-prefix
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-subject
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-statement
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)
(s/fdef kn/read-annotation
        :args (s/cat :env ::en/env :parse ::parse)
        :ret ::st/state)

(s/fdef kn/read-object
        :args (s/cat :env ::en/env :parse ::parse :predicate ::rdf/predicate :language (s/nilable ::rdf/language) :datatype (s/nilable ::rdf/datatype))
        :ret map?)

(s/fdef kn/process-states
        :args (s/cat :states ::st/states)
        :ret ::st/states)
(s/fdef kn/expand-state
        :args (s/cat :env ::en/env :state ::st/state)
        :ret (s/tuple ::st/state ::parses))

(s/fdef kn/render-state
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-blank
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-comment
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-prefix
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-subject
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-statement
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)
(s/fdef kn/render-annotation
        :args (s/cat :env ::en/env :state ::st/state)
        :ret ::parse)

(s/fdef kn/render-datatype
        :args (s/cat :env ::en/env :predicate ::rdf/predicate :object map?)
        :ret ::parse)
(s/fdef kn/render-object
        :args (s/cat :env ::en/env :object map?)
        :ret ::parse)
