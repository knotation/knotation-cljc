(ns org.knotation.link-spec
  (:require [#?(:clj clojure.spec.alpha :cljs cljs.spec.alpha) :as s]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdf-spec]
            [org.knotation.environment :as en]
            [org.knotation.environment-spec]
            [org.knotation.link :as ln]))

(s/fdef label->iri
        :args (s/cat :env ::en/env :input string?)
        :ret (s/nilable ::rdf/iri))
(s/fdef wrapped-iri->iri
        :args (s/cat :env ::en/env :input string?)
        :ret (s/nilable ::rdf/iri))
(s/fdef http-url->iri
        :args (s/cat :env ::en/env :input string?)
        :ret (s/nilable ::rdf/iri))
(s/fdef curie->iri
        :args (s/cat :env ::en/env :input string?)
        :ret (s/nilable ::rdf/iri))

(s/fdef wrapped-iri-or-bnode->node
        :args (s/cat :input string?)
        :ret (s/nilable ::rdf/link-node))

(s/fdef subject->iri
        :args (s/cat :env ::en/env :input string?)
        :ret ::rdf/iri)
(s/fdef predicate->iri
        :args (s/cat :env ::en/env :input string?)
        :ret ::rdf/iri)

(s/fdef subject->node
        :args (s/cat :env ::en/env :input string?)
        :ret ::rdf/link-node)

(s/fdef object->node
        :args (s/cat :env ::en/env :input string?)
        :ret ::rdf/node)

(s/fdef find-prefix
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable string?))

(s/fdef iri->curie
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable string?))
(s/fdef iri->http-url
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret (s/nilable string?))
(s/fdef iri->wrapped-iri
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret string?)
(s/fdef iri->name
        :args (s/cat :env ::en/env :iri ::rdf/iri)
        :ret string?)

(s/fdef node->name
        :args (s/cat :env ::en/env :iri ::rdf/link-node)
        :ret (s/nilable string?))
