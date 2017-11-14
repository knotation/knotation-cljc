(ns org.knotation.environment-spec
  (:require [clojure.spec.alpha :as s]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(s/def ::en/label string?)
(s/def ::en/prefix string?) ; TODO

(s/def ::en/prefix-iri (s/map-of ::en/prefix ::rdf/iri))
(s/def ::en/iri-prefix (s/map-of ::rdf/iri ::en/prefix))
(s/def ::en/prefix-seq (s/* (s/tuple ::en/prefix ::rdf/iri)))
(s/def ::en/sorted-prefix-seq ::en/prefix-seq)

(s/def ::en/label-iri (s/map-of ::en/label ::rdf/iri))
(s/def ::en/iri-label (s/map-of ::rdf/iri ::en/label))
(s/def ::en/label-seq (s/* (s/tuple ::en/label ::rdf/iri)))

(s/def ::en/predicate-datatype (s/map-of ::rdf/predicate ::rdf/iri))

(s/def ::en/env map?)
(s/def ::en/env-before ::en/env)

(s/fdef en/add-prefix
        :args (s/cat :env ::en/env :prefix string? :iri ::rdf/iri)
        :ret ::en/env)

(s/fdef en/add-label
        :args (s/cat :env ::en/env :label string? :iri ::rdf/iri)
        :ret ::en/env)

(s/fdef en/add-datatype
        :args (s/cat :env ::en/env :predicate ::rdf/iri ::datatype ::rdf/iri)
        :ret ::en/env)
